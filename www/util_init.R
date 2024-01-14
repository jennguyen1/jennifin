options(readr.show_col_types = FALSE)

## some date stuff ##

today <- Sys.Date()
year_start <- "2024-01-02"


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

calc_rsi_14 <- function(ticker, i = 1:252){
  use_ticker <- clean_ticker(ticker)
  
  tryCatch({
    tidyquant::tq_get(use_ticker, from = today - 252*2) %>% 
      dplyr::mutate(rsi = TTR::RSI(close, n = 14)) %>% 
      dplyr::arrange(dplyr::desc(date)) %>% 
      dplyr::select(date, rsi) %>% 
      head(252) %>% 
      dplyr::slice(i) %>% 
      dplyr::pull(rsi) 
  }, error = function(e) NA
  )
}

get_rsi_stats <- function(ticker){
  rsi <- calc_rsi_14(ticker)
  data.frame(
    rsi = rsi[1],
    rsi2 = rsi[10], # todo
    days_since_os = dplyr::coalesce(which(rsi < 30)[1] - 1, 252),
    days_since_ob = dplyr::coalesce(which(rsi > 70)[1] - 1, 252)
  )
}

get_sma_slope <- function(ticker, n = 200){
  use_ticker <- clean_ticker(ticker)
  
  tryCatch({
    sma <- tidyquant::tq_get(use_ticker, from = today - 756) %>%
      dplyr::mutate(sma = TTR::SMA(close, n = n)) %>%
      tail(11) %>% 
      dplyr::pull(sma)
    
    b4 <- head(sma, 1)
    af <- tail(sma, 1)
    (af - b4) / 10 / b4 * 100
  }, error = function(e) NA
  )
}

get_sma_slope_direction <- function(ticker, n = 200){
  slope <- get_sma_slope(ticker, n)
  
  dplyr::case_when(
    slope > 0.01 ~ "+", 
    slope < -0.01 ~ "-",
    TRUE ~ "0"
  )
}

# todo: put avwap slope info into asc
# avwap 
get_ticker_data <- function(ticker, df_dates){
  use_ticker <- clean_ticker(ticker)
  use_date <- min(as.Date(df_dates))
  tidyquant::tq_get(use_ticker, from = use_date) 
}

calculate_avwap <- function(dat){
  dat %>% 
    dplyr::mutate(
      price = (open+high+low+close)/4, 
      num = cumsum(price * volume),
      den = cumsum(volume),
      avwap = num/den 
    ) %>% 
    dplyr::pull(avwap) %>% 
    tail(1)
}

get_avwap_series <- function(ticker, df_dates){
  tryCatch({
    df <- get_ticker_data(ticker, df_dates$date)
    df_dates %>% 
      dplyr::mutate(
        avwap = purrr::map_dbl(date, function(dt){
          df %>% 
            dplyr::filter(date >= dt) %>% 
            calculate_avwap()
        })
      ) %>% 
      dplyr::select(label, avwap) %>% 
      tidyr::pivot_wider(names_from = label, values_from = avwap)
  }, error = function(e){
    data.frame()
  })
}


### table creation ###

clean_data_etfs <- function(dat){
  
  # for avwap
  date1 <- as.character(purrr::discard(unique(dat$date_1), is.na))
  date2 <- as.character(purrr::discard(unique(dat$date_2), is.na))
  
  dates <- data.frame(
    label = c("avwap_ytd", "avwap_anchor_1", "avwap_anchor_2"),
    date = c(year_start, date1, date2)
  )
  
  # cleaning
  dat %>% 
    dplyr::mutate(
      avwaps = purrr::map(ticker, ~ get_avwap_series(.x, dates))
    ) %>% 
    tidyr::unnest(avwaps) %>% 
    dplyr::mutate(
      return_1m = pdiff(price, price_1m),
      return_3m = pdiff(price, price_3m),
      return_6m = pdiff(price, price_6m),
      return_1y = pdiff(price, price_12m),
      return_ytd = pdiff(price, price_year_start),
      return_50d = pdiff(price, price_50d),
      return_200d = pdiff(price, price_200d),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      return_anchor_1 = pdiff(price, price_anchor_1),
      return_anchor_2 = pdiff(price, price_anchor_2),
      return_avwap_anchor_1 = pdiff(price, avwap_anchor_1),
      return_avwap_anchor_2 = pdiff(price, avwap_anchor_2),
      return_avwap_ytd = pdiff(price, avwap_ytd)
    ) %>% 
    dplyr::select(
      -dplyr::starts_with("price"), 
      -dplyr::starts_with("avwap"), 
      -dplyr::starts_with("date")
    )
}

clean_data_stocks <- function(dat){
  
  # for avwap
  date1 <- as.character(purrr::discard(unique(dat$date_1), is.na))
  date2 <- as.character(purrr::discard(unique(dat$date_2), is.na))
  
  dates <- data.frame(
    label = c("avwap_ytd", "avwap_anchor_1", "avwap_anchor_2"),
    date = c(year_start, date1, date2)
  )
  
  # cleaning
  dat %>% 
    dplyr::mutate(
      avwaps = purrr::map(ticker, ~ get_avwap_series(.x, dates))
    ) %>% 
    tidyr::unnest(avwaps) %>% 
    dplyr::mutate(
      return_20d = pdiff(price, price_20d),
      return_50d = pdiff(price, price_50d),
      return_200d = pdiff(price, price_200d),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      return_1m = pdiff(price, price_1m),
      return_ytd = pdiff(price, price_year_start),
      return_anchor_1 = pdiff(price, price_anchor_1),
      return_anchor_2 = pdiff(price, price_anchor_2),
      return_avwap_anchor_1 = pdiff(price, avwap_anchor_1),
      return_avwap_anchor_2 = pdiff(price, avwap_anchor_2),
      return_avwap_ytd = pdiff(price, avwap_ytd)
    ) %>% 
    dplyr::select(
      -dplyr::starts_with("price"), 
      -dplyr::starts_with("avwap"), 
      -dplyr::starts_with("date")
    )
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
    
  # todo: last section here
  # dplyr::filter(return_anchor_1 >= 0) %>% # at or above anchor DATE high (target something like 52w high for SP1500), note this may change
  # dplyr::filter(days_since_os > 21*3) %>% # remove if oversold in the last 3m
    
  # add rsi
  d2 <- d1 %>%
    dplyr::mutate(rsi = purrr::map(ticker, get_rsi_stats)) %>%
    tidyr::unnest(rsi) 
  
  d3 <- d2 %>%
    dplyr::select(
      ticker, 
      dplyr::one_of(c("company", "sector", "industry", "size")), 
      dplyr::matches("anchor_1"), return_avwap_ytd, 
      days_since_os, 
      dplyr::matches("52")
    )

  # sort by 52w highs
  d3 %>% dplyr::arrange(dplyr::desc(below_52w_high))
}

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

collect_ma_breadth_stats <- function(stocks){
  
  date <- as.Date(stringr::str_trim(readr::read_file("data/read_time.txt")))
  file <- "data/stats_ma_50d_gr_200d.csv"
  prev_data <- readr::read_csv(file)
  
  # current week data
  row_add <- stocks %>% 
    dplyr::mutate(date = date) %>% 
    dplyr::group_by(date, size) %>% # 50d SMA > 200d SMA (inversely correlated with return %)
    dplyr::summarise(p = round(mean(return_50d < return_200d, na.rm = TRUE)*100, 1))
  
  # save
  dplyr::bind_rows(prev_data, row_add) %>% 
    dplyr::distinct() %>% 
    readr::write_csv(file)
}
