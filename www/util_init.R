options(readr.show_col_types = FALSE)


## some date stuff ##

today <- Sys.Date()
year_start <- "2024-01-02"

# note this may change
anchor_1 <- "2023-07-31"
anchor_2 <- "2023-10-27"
anchor_1_msg <- "Jul High"
anchor_2_msg <- "Oct Low"
anchor_msg <- anchor_1_msg


### general functions ###
pdiff <- function(x, base) round((x-base)/base, 3) 

clean_ticker <- function(ticker){
  plyr::mapvalues(
    ticker, # ticker transcription if needed
    c("BRKB", "MOG.A", "BF.B"), 
    c("BRK-B", "MOG-A", "BF-B"), 
    warn_missing = FALSE
  )
}

price_on_date <- function(dat, dt, type = "close"){
  val <- dat %>% 
    dplyr::filter(date >=  dt) %>% 
    head(1) %>% 
    .[ , type, drop = TRUE]
  
  ifelse(length(val) == 0, NA, val)
}

price_on_ytd <- function(dat){
  dat %>% 
    dplyr::filter(year(date) < year(today)) %>%  
    tail(1) %>% 
    dplyr::pull(close)
}

calculate_avwap <- function(dat, dt){
  dat %>% 
    dplyr::filter(date >= dt) %>% 
    dplyr::mutate(
      price = (open+high+low+close)/4, 
      num = cumsum(price * volume),
      den = cumsum(volume),
      avwap = num/den 
    ) %>% 
    dplyr::pull(avwap) %>% 
    tail(1)
}

get_rsi_stats <- function(dat){

  rsi <- dat %>% 
    dplyr::arrange(dplyr::desc(date)) %>% 
    dplyr::select(date, rsi) %>% 
    dplyr::pull(rsi) 
  
  data.frame(
    rsi = rsi[1],
    days_since_os = dplyr::coalesce(which(rsi < 30)[1] - 1, 252),
    days_since_ob = dplyr::coalesce(which(rsi > 70)[1] - 1, 252)
  )
}


### data creation ###
query_ticker_data <- function(dat){
  
  d0 <- dat$ticker %>% 
    clean_ticker() %>% 
    tidyquant::tq_get(from = "2020-01-01") %>% 
    dplyr::rename(ticker = symbol)
  
  dplyr::full_join(dat, d0, "ticker")
}

create_ta_columns <- function(dat){
  dat %>% 
    tidyr::nest(data = -ticker) %>% 
    dplyr::mutate(
      data = purrr::map(data, function(d){
        tryCatch({
          d %>% 
            dplyr::mutate(
              rsi = TTR::RSI(close, n = 14),
              ma_20 = TTR::SMA(close, 20),
              ma_50 = TTR::SMA(close, 50),
              ma_200 = TTR::SMA(close, 200)
            )
        }, error = function(e){
          dplyr::mutate(d, rsi = NA, ma_20 = NA, ma_50 = NA, ma_200 = NA)
        })
      })
    ) %>% 
    tidyr::unnest(data)
}

create_price_columns <- function(dat, anchor_1, anchor_2){
  
  dat_sub_1y <- tail(dat, 252) # 1y
  
  out <- dat_sub_1y %>% 
    dplyr::summarise(
      price = tail(.$close, 1),
      # hi/lo
      price_above_52w_lo = min(low), 
      price_below_52w_hi = max(high),
      # ma
      price_200d = tail(.$ma_200, 1), 
      price_50d = tail(.$ma_50, 1), 
      price_20d = tail(.$ma_20, 1), 
      # price from date
      price_1m = price_on_date(., today - days(31)),
      price_3m = price_on_date(., today - days(91)),
      price_6m = price_on_date(., today - days(181)),
      price_12m = price_on_date(., today - days(366)),
      price_ytd = price_on_ytd(.),
      # price anchors
      price_anchor_1 = price_on_date(dat, anchor_1), 
      price_anchor_2 = price_on_date(dat, anchor_2), 
      price_avwap_anchor_1 = calculate_avwap(dat, anchor_1),
      price_avwap_anchor_2 = calculate_avwap(dat, anchor_2),
      price_avwap_ytd = calculate_avwap(dat, year_start)
    )
  
  rsi <- get_rsi_stats(dat_sub_1y)
  
  # combine
  dplyr::bind_cols(out, rsi)
}

clean_data <- function(dat, ...){
  
  dat %>% 
    tidyr::nest(data = -c(ticker, ...)) %>% 
    dplyr::mutate(res = purrr::map(data, create_price_columns, anchor_1 = anchor_1, anchor_2 = anchor_2)) %>% 
    tidyr::unnest(res) %>% 
    dplyr::select(-data) %>% 
    dplyr::mutate(dplyr::across(
      dplyr::starts_with("price_"), 
      function(x) pdiff(price, x)
    )) %>% 
    purrr::set_names(colnames(.) %>% stringr::str_replace("price_", "return_"))
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
    dplyr::filter(return_avwap_anchor_1 >= 0) %>% # at or above avwap from anchor date high (target something like 52w high for stocks), may change
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
  d2 %>% dplyr::arrange(dplyr::desc(return_below_52w_hi))
}


### data collection ### 
collect_ta_stats <- function(stocks, stocks_ta){
  
  date <- as.Date(stringr::str_trim(readr::read_file("data/read_time.txt")))
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

