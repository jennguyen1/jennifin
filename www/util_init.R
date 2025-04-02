options(readr.show_col_types = FALSE)
source("www/db_executions.R")


### general functions ###
pdiff <- function(x, base) round((x-base)/base, 3) 

get_latest_date <- function(){
  query_db("SELECT MAX(date) as date FROM prices WHERE ticker = 'SPY'")$date
}


### db update ###
prep_for_db_upload <- function(dat, d_old){
  d_rsi <- dat %>% 
    dplyr::mutate(date = as.Date(date)) %>% 
    tidyr::nest(data = -ticker) %>% 
    dplyr::mutate(data = purrr::map(data, function(d){
      d %>% 
        dplyr::arrange(date) %>%
        dplyr::mutate(rsi = tryCatch(TTR::RSI(close, n = 14), error = function(e) NA))
    })) %>% 
    tidyr::unnest(data) 
  
  dplyr::anti_join(d_rsi, d_old %>% dplyr::mutate(date = as.Date(date)), c("ticker", "date")) %>% 
    dplyr::filter(!is.na(close))
}

add_to_price_db <- function(dat, db = use_db){
  db_conn <- DBI::dbConnect(duckdb::duckdb(), db)
  tryCatch({
    DBI::dbWriteTable(db_conn, "prices", dat, append = TRUE)
    message("Data Uploaded to DB")
  }, 
  finally = {
    DBI::dbDisconnect(db_conn)
  })
}

run_db_update <- function(){
  reticulate::source_python("www/extract_price_data.py")
  d_ready_upload <- prep_for_db_upload(d_out, d_old)
  add_to_price_db(d_ready_upload)
}

execute_in_db <- function(query, db = use_db){
  db_conn <- DBI::dbConnect(duckdb::duckdb(), db)
  tryCatch({
    DBI::dbExecute(db_conn, query)
  }, 
  finally = {
    DBI::dbDisconnect(db_conn)
  })
}

add_view_avwap <- function(dt_name, dt, db = use_db){
  query <- stringr::str_glue(view_creation['avwap'])
  execute_in_db(query, db = db)
}

add_view_price_anchor <- function(dt_name, dt, db = use_db){
  query <- stringr::str_glue(view_creation['price_anchor'])
  execute_in_db(query, db = db)
}

query_db <- function(query, db = use_db){
  db_conn <- DBI::dbConnect(duckdb::duckdb(), db)
  tryCatch({
    DBI::dbGetQuery(db_conn, query)
  }, 
  finally = {
    DBI::dbDisconnect(db_conn)
  })
}


### data creation ###
convert_to_return <- function(dat){
  dat %>% 
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("price_"), 
      function(x) pdiff(price, x)
    )) %>% 
    purrr::set_names(colnames(.) %>% stringr::str_replace("price_", "return_"))
}

get_data <- function(table, pull_date, db = use_db){
  assertthat::assert_that(table %in% c("etfs", "stocks"))
  
  pull_date <- get_latest_date()
  specific_columns <- if(table == "etfs"){
    "etfs.ticker, etfs.description as desc, etfs.e_type as type, etfs.category, etfs.category2"
  } else if(table == "stocks"){
    "stocks.ticker, stocks.company, stocks.sector, stocks.industry, stocks.size"
  }
  
  query_db(stringr::str_glue(assemble_data), db) %>% 
    convert_to_return()
}

apply_technical_screen <- function(dat, etfs){
  
  spy_1m <- dplyr::filter(etfs, ticker == "SPY")$return_1m
  
  sector_1m <- data.frame(
    ticker = paste0("RSP", c("F", "N", "M", "G", "D", "T", "C", "R", "S", "U", "H")), 
    sector = c(
      "Financial", 
      "Industrial", 
      "Materials", 
      "Energy", 
      "Consumer Discretionary", 
      "Technology", 
      "Communication Services", 
      "Real Estate",
      "Consumer Staples", 
      "Utilities", 
      "Health Care"
    )
  ) %>% 
    dplyr::left_join(etfs, "ticker") %>% 
    dplyr::select(sector, return_1m)
  
  # initial filter based 50D/200D uptrend, S/R, RS to spy / sector
  d1 <- dat %>% 
    dplyr::filter(return_50d > 0 & return_200d > 0 & return_50d < return_200d) %>%  # keep above 50d & 200d SMA and 50d SMA > 200d SMA
    dplyr::filter(return_avwap_anchor_1 >= 0) %>% # at or above avwap from anchor date high (target something like 52w high for stocks), note may change
    dplyr::filter(return_1m - spy_1m > -0.01) %>% # remove laggards to SPY over last 1m
    dplyr::left_join(sector_1m, "sector", suffix = c("", "_sect")) %>% 
    dplyr::filter(return_1m - return_1m_sect > -0.01)
  
  d2 <- d1 %>%
    dplyr::select(
      ticker, 
      dplyr::one_of(c("company", "sector", "industry", "size")), 
      dplyr::matches("anchor_1"), return_avwap_ytd, 
      days_since_os, 
      dplyr::matches("52")
    )
  
  # sort by 52w highs
  d2 %>% dplyr::arrange(dplyr::desc(return_52w_hi))
}


### data collection ### 
collect_ta_stats <- function(stocks, stocks_ta, date){
  
  file <- "data/stats_ta_screen.csv"
  prev_data <- readr::read_csv(file)
  
  # current week data
  num <- dplyr::count(stocks_ta, sector)
  den <- dplyr::count(stocks, sector)
  row_add <- dplyr::full_join(num, den, "sector") %>% 
    dplyr::mutate(
      date = date,
      sector = sector %>% stringr::str_to_lower() %>% stringr::str_replace_all("\\s+", "_"),
      p = round(n.x/n.y*100, 1) %>% tidyr::replace_na(0)
    ) %>% 
    dplyr::select(date, sector, p) %>% 
    tidyr::pivot_wider(names_from = sector, values_from = p)
  
  # save
  dplyr::bind_rows(prev_data, row_add) %>% 
    dplyr::distinct() %>% 
    readr::write_csv(file)
}


### breadth animation ###
animate_breadth <- function(yr = 2020){
  library(gganimate)
  
  df <- query_db(stringr::str_glue("
    SELECT 
      s.ticker, s.sector, s.size, sectors.category,
      p.date, p.close, p.price_20d, p.price_50d, p.price_200d
    FROM stocks as s
    LEFT JOIN price_stats as p
      ON s.ticker = p.ticker
    LEFT JOIN sectors
      ON s.sector = sectors.sector
    WHERE date >= '{yr}-01-01'
  ")) %>% 
    dplyr::filter(wday(date) %in% c(2,4,6)) %>% 
    dplyr::mutate(dplyr::across(dplyr::matches("_\\d+d$"), \(x) close > x))
  
  sdf <- df %>% # by size
    dplyr::group_by(date, size) %>% 
    dplyr::summarise(across(
      dplyr::ends_with("0d"),
      \(x) mean(x, na.rm = TRUE)
    )) %>% 
    dplyr::mutate(category = "all") %>% 
    dplyr::rename(sector = size)
  
  adf <- df %>% # by sector
    dplyr::group_by(date, category, sector) %>% 
    dplyr::summarise(dplyr::across(
      dplyr::ends_with("0d"),
      \(x) mean(x, na.rm = TRUE)
    ))
  
  plot_df <- dplyr::bind_rows(sdf, adf) %>% 
    tidyr::pivot_longer(-c(date, category, sector)) %>% 
    dplyr::mutate(
      days = factor(readr::parse_number(name)),
      sector = plyr::mapvalues(
        sector, 
        c("Consumer Staples", "Communication Services", "Consumer Discretionary"), 
        c("Staples", "Communications", "Discretionary")
      ),
      category = factor(stringr::str_to_title(category), levels = c("All", "Growth", "Cyclical", "Defensive"))
    ) 
  
  # animate graph
  a <- plot_df %>% 
    ggplot() +
    geom_hline(yintercept = c(0, 100), color = "grey50") +
    geom_hline(yintercept = 50, color = "grey50", linetype = "dashed") +
    geom_line(aes(days, value*100, group = sector, color = sector), linewidth = 1.25) +
    geom_text(
      data = plot_df %>% dplyr::filter(days == 200),
      aes(3.1, value*100, label = sector, color = sector),
      hjust = 0
    ) +
    facet_grid(~category) + 
    scale_y_continuous(breaks = seq(0, 100, 10)) + 
    scale_color_manual(values = c(
      "deepskyblue", "dodgerblue", "darkgreen", "yellowgreen", "red", "green3", "black", 
      "green4", "grey30", "hotpink1", "grey60", "firebrick", "blue", "coral1"
    )) +
    expand_limits(x = 4.75) +
    labs(x = "Moving Average", y = "% of Stocks Above", color = "") + 
    theme_bw(base_family = "Arial", base_size = 16) + 
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    transition_manual(frames = date) +
    labs(title = '{current_frame}')
  
  nf <- plot_df %>% count(date) %>% nrow()
  animate(
    a, 
    nframes = nf, fps = 6,
    height = 6, width = 10, units = "in", 
    res = 110, 
    end_pause = 30
  )
  
  anim_save("www/ma_breadth.gif")
  file.remove("www/ma_breadth.mp4")
  system("ffmpeg -i www/ma_breadth.gif -movflags faststart -pix_fmt yuv420p -vf 'scale=trunc(iw/2)*2:trunc(ih/2)*2' www/ma_breadth.mp4")
  file.remove("www/ma_breadth.gif")
}

