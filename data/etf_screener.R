
library(tidyverse)
source("www/util_init.R")

Sys.time() # typically 10-15 min
e <- readr::read_csv("data/tickers_etf.csv") %>% 
  query_ticker_data() %>% 
  create_ta_columns()
s <- readr::read_csv("data/tickers_stock.csv") %>% 
  query_ticker_data() %>% 
  create_ta_columns()
Sys.time()

etfs <- clean_data(e, desc, type, category, category2)
stocks <- clean_data(s, company, sector, industry, size)
stocks_ta_screen <- apply_technical_screen(stocks, etfs)

# save output
save(e, s, etfs, stocks, stocks_ta_screen, file = "data/data.RData")

# data collection
collect_ta_stats(stocks = stocks, stocks_ta = stocks_ta_screen, date = max(s$date, na.rm = TRUE))

# breadth animation
animate_breadth(s)


### Every Quarter - SP Companies ###

# pull data from stockcharts - get sector & industry
# now is pre-built as example scan
# [group is SP500] OR [group is SP400] OR [group is SP600]

# other stocks not in SP1500 - requires a little cleanup (rm na's)
# [type is stock] AND [group is not etf] AND
# [group is not SP500] AND [group is not SP400] AND [group is not SP600] AND 
# [[exchange is NYSE  ] OR [exchange is NASDAQ]] AND
# [ market cap > 1000 ] 


# temporary to see if this is useful
# if keep highlight diff green
get_history <- function(dat){
  if(all(is.na(dat$close))) return(NULL)
  days <- c(252, 126, 63, 21, 10)
  high <- purrr::map_dbl(days, ~ max(tail(dat$close, .x), na.rm = TRUE))
  low <- purrr::map_dbl(days, ~ min(tail(dat$close, .x), na.rm = TRUE))
  
  data.frame(days, high, low) %>% 
    dplyr::mutate(today = tail(dat$close, 1))
}

make_high_low_table <- function(s, dt){
  s %>% 
    dplyr::filter(date <= as.Date(dt)) %>% 
    tidyr::nest(data = -ticker) %>%
    dplyr::mutate(a = suppressWarnings(purrr::map(data, get_history))) %>%
    tidyr::unnest(a) %>%
    dplyr::group_by(days) %>%
    dplyr::summarise(
      lo = sum(low == today, na.rm = TRUE),
      hi = sum(high == today, na.rm = TRUE),
      diff = hi - lo
    )
}

make_high_low_table(s, today())
# high/low table
