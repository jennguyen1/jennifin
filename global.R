library(tidyverse)
theme_set(theme_bw(base_family = "Arial", base_size = 16))

# read files
load("data/data0.RData")
load("data/data.RData")
get_file_update_dt <- function() max(e$date) # todo

source('www/util.R')
