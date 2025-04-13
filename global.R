library(tidyverse)
theme_set(theme_bw(base_family = "Arial", base_size = 16))
today <- Sys.Date()

use_db <- "data/stock_prices.db"

source("www/util_init.R")
source('www/util.R')

# note this may change 
anchor_1 <- "2025-02-19"
anchor_2 <- "2025-04-07"
anchor_1_msg <- "Feb High"
anchor_2_msg <- "Apr Low"

# pull data
etfs <- get_data("etfs", get_latest_date())
stocks <- get_data("stocks", get_latest_date())
sectors <- query_db("SELECT * from sectors")
stocks_ta_screen <- apply_technical_screen(stocks, etfs)
collect_ta_stats(stocks = stocks, stocks_ta = stocks_ta_screen, date = get_latest_date())
