
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
save(e, s, file = "data/data0.RData")
save(etfs, stocks, stocks_ta_screen, file = "data/data.RData")

# data collection
collect_ta_stats(stocks = stocks, stocks_ta = stocks_ta_screen)


### Every Quarter - SP Companies ###
# The source is www.ssga.com
# tidyquant::tq_index("SP500") %>% readr::write_csv("~/Downloads/lc.csv")
# tidyquant::tq_index("SP400") %>% readr::write_csv("~/Downloads/mc.csv")
# tidyquant::tq_index("SP600") %>% readr::write_csv("~/Downloads/sc.csv")

# pull data from stockcharts - get sector & industry
# [group is SP500] OR [group is SP400] OR [group is SP600]

# other stocks not in SP1500 - requires a little cleanup (rm na's)
# [type is stock] AND [group is not etf] AND
# [group is not SP500] AND [group is not SP400] AND [group is not SP600] AND 
# [[exchange is NYSE  ] OR [exchange is NASDAQ]] AND
# [ market cap > 1000 ] 

