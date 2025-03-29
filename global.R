library(tidyverse)
theme_set(theme_bw(base_family = "Arial", base_size = 16))
today <- Sys.Date()

# read files
source("www/util_init.R")
etfs <- get_data("etfs")
stocks <- get_data("stocks")
stocks_ta_screen <- apply_technical_screen(stocks, etfs)
# collect_ta_stats(stocks = stocks, stocks_ta = stocks_ta_screen, date = get_latest_date()) todo

source('www/util.R')
