library(tidyverse)

# read files
etf_file <- "data/etfs.csv"
stock_file <- "data/stocks.csv"
etfs <- readr::read_csv(etf_file)
stocks <- readr::read_csv(stock_file) 

# recent file update
get_file_update_dt <- function(){
  dt <- file.info( c(etf_file, stock_file) ) %>% 
    dplyr::pull(mtime) %>% 
    min() 
  
  date <- as.Date(dt)
  time <- format(dt, "%H:%M:%S") %>% 
    strptime(format = "%H:%M:%S") %>% 
    format("%I:%M %p")
  timezone <- format(dt, "%Z")
  
  paste(date, time, timezone)
}