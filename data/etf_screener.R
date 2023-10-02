library(googlesheets4)
library(tidyverse)
source("www/util_init.R")
file <- "1kOpm7h3UqQ1Onen3woMDu__-4j5on0Cdd2ZgvBuMxSM"
# file <- "1FlxfmUhs8tLDiTRGK2qpBiIx1fVBMZs-iyL2DcQYGrY"

### Run Before Each Session - Refresh Prices ###


# ETFs
(e <- googlesheets4::read_sheet(file, "ETFs") %>% clean_data_etfs())

readr::write_csv(e, "data/etfs.csv")


# Stocks
(s500 <- googlesheets4::read_sheet(file, "SP500") )
(s400 <- googlesheets4::read_sheet(file, "SP400") )
(s600 <- googlesheets4::read_sheet(file, "SP600") )
s <- dplyr::bind_rows(s500, s400, s600) %>% clean_data_stocks() 
s_ta <- apply_technical_screen(s, e)

readr::write_csv(s, "data/stocks.csv")
readr::write_csv(s_ta, "data/stocks_ta_screen.csv")

# file write info
write_file_update <- function(){
  dt <- file.info( c("data/etfs.csv", "data/stocks.csv") ) %>% 
    dplyr::pull(mtime) %>% 
    min() 
  
  date <- stringr::str_extract(dt, "\\d{4}-\\d{2}-\\d{2}")
  time <- format(dt, "%H:%M:%S") %>% 
    strptime(format = "%H:%M:%S") %>% 
    format("%I:%M%p")
  timezone <- format(dt, "%Z")
  
  paste(date, time, "EST") %>% 
    readr::write_lines("data/read_time.txt")
}
write_file_update()


# data collection
collect_ta_stats(stocks = s, stocks_ta = s_ta)
collect_ma_breadth_stats(stocks = s)
collect_hilo_breadth_stats(stocks = s)


### Every Quarter - SP Companies ###
# The source is www.ssga.com
# tidyquant::tq_index("SP500") %>% readr::write_csv("~/Downloads/lc.csv")
# tidyquant::tq_index("SP400") %>% readr::write_csv("~/Downloads/mc.csv")
# tidyquant::tq_index("SP600") %>% readr::write_csv("~/Downloads/sc.csv")

# pull data from stockcharts - get sector & industry
# [
#   [group is SP500] OR [group is SP400] OR [group is SP600]
# ]

