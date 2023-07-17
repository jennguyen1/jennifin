options(readr.show_col_types = FALSE)

## some date stuff ##

today <- Sys.Date()


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


### table creation ###

clean_data_etfs <- function(dat){
  dat %>% 
    dplyr::mutate(
      return_1m = pdiff(price, price_1m),
      return_3m = pdiff(price, price_3m),
      return_6m = pdiff(price, price_6m),
      return_1y = pdiff(price, price_12m),
      return_ytd = pdiff(price, price_year_start),
      return_50d = pdiff(price, price_50d),
      slope_50d = purrr::map_chr(ticker, get_sma_slope_direction, n = 50),
      return_200d = pdiff(price, price_200d),
      slope_200d = purrr::map_chr(ticker, get_sma_slope_direction, n = 200),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      return_anchor_lo = pdiff(price, price_anchor_lo),
      return_anchor_hi = pdiff(price, price_anchor_hi)
    ) %>% 
    dplyr::select(-dplyr::starts_with("price"), -dplyr::starts_with("date"))
}

clean_data_stocks <- function(dat){
  dat %>% 
    dplyr::mutate(
      return_20d = pdiff(price, price_20d),
      return_50d = pdiff(price, price_50d),
      return_200d = pdiff(price, price_200d),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      return_1m = pdiff(price, price_1m),
      return_ytd = pdiff(price, price_year_start),
      return_anchor_lo = pdiff(price, price_anchor_lo),
      return_anchor_hi = pdiff(price, price_anchor_hi)
    ) %>% 
    dplyr::select(-dplyr::starts_with("price"), -dplyr::starts_with("date"))
}

apply_technical_screen <- function(dat, etfs){
  spy_1m <- dplyr::filter(etfs, ticker == "SPY")$return_1m
  
  # initial filter based 200D, S/R & RS to spy
  d1 <- dat %>% 
    dplyr::filter(return_200d > 0) %>%  # keep above 200d SMA
    dplyr::filter(return_anchor_hi >= 0) %>% # at or above anchor DATE high (target something like 52w high for SP1500)
    dplyr::filter(return_1m > spy_1m)  # remove laggards to SPY over last 1m
  
  # filter those in bullish rsi regime
  d2 <- d1 %>% 
    dplyr::mutate(
      sma200_slope = purrr::map_dbl(ticker, get_sma_slope),
      rsi = purrr::map(ticker, get_rsi_stats)
    ) %>% 
    tidyr::unnest(rsi) %>% 
    dplyr::filter(sma200_slope > 0) %>% # keep if 200d SMA slope positive over past 2 weeks
    dplyr::filter(days_since_os > 21*3) %>% # remove if oversold in the last 3m
    dplyr::select(ticker, dplyr::one_of(c("company", "sector", "industry", "size")), return_200d, dplyr::matches("52"), days_since_os) 
  
  # sort by 52w highs
  d2 %>% dplyr::arrange(dplyr::desc(below_52w_high))
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
      p = round(n.x/n.y*100, 1)
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
