library(tidyverse)
theme_set(theme_bw(base_family = "Arial", base_size = 16))

# read files
etfs <- readr::read_csv("data/etfs.csv")
stocks <- readr::read_csv("data/stocks.csv") 
stocks_ta_screen <- readr::read_csv("data/stocks_ta_screen.csv")

get_file_update_dt <- function(){
  stringr::str_trim(readr::read_file("data/read_time.txt"))
}

source('www/util.R')
