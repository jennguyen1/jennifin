library(tidyverse)
source('www/util_init.R')
source('www/util.R')


# avwap updates ----------------------------------------------------------

googlesheets4::gs4_auth("jennifernguyen1992@gmail.com")
file <- "12Tv_-fBoLMc1hcyTWw5jEzd_z3P5Q-uhjHp12j1WlTo"
df <- googlesheets4::read_sheet(file, "Watchlist")

dates <- data.frame(label = "t", date = "2024-01-02")

odf <- df %>% 
  dplyr::mutate(
    res = purrr::map(Ticker, function(x){
      tryCatch({
        tidyquant::tq_get(clean_ticker(x), from = "2024-01-02") %>% 
          dplyr::summarise(
            avwap = calculate_avwap(., "2024-01-02"), 
            yday = tail(., 1)$close
          )
      }, error = function(e) data.frame(avwap = NA, yday = NA))
    })
  ) %>% 
  dplyr::select(Ticker, res) %>% 
  tidyr::unnest(res) 

assertthat::assert_that(nrow(df) == nrow(odf))
googlesheets4::write_sheet(odf, file, 'add_avwaps')

# [ACP list is 243984] and [next earnings date < 20240218] and [next earnings date > 20240201]

# etf screen -------------------------------------------------------------------

spy_1m <- dplyr::filter(etfs, ticker == "SPY")$return_1m
etf_f <- etfs %>% 
  dplyr::filter(return_50d > 0 & return_200d > 0 & return_50d < return_200d) %>% 
  dplyr::filter(return_avwap_anchor_1 >= 0) %>% 
  dplyr::filter(return_1m > spy_1m) %>% 
  # dplyr::mutate(rsi = purrr::map(ticker, get_rsi_stats)) %>%
  # tidyr::unnest(rsi) %>% 
  dplyr::select(
    ticker, desc, type, category, category2, 
    return_anchor_1, return_avwap_anchor_1,
    return_anchor_2, return_avwap_anchor_2,
    days_since_os, 
    dplyr::matches("52")
  ) %>% 
  dplyr::arrange(dplyr::desc(return_below_52w_hi)) 

etf_f %>% View()


# other stocks ----------------------------------------------------------------
# todo
# (os <- googlesheets4::read_sheet("10oKFDmMz0UY78I4ARn2JKblJsajIcw2Eda3O-Afqpxs"))
# os_clean <- clean_data_stocks(os)
# scan2 <- apply_technical_screen(os_clean)
# 
# plotly::ggplotly(
#   os %>%
#     subset(ticker %in% scan2$ticker) %>%
#     graph_anchor_scatter(sector)
# )
# 
# plotly::ggplotly(
#   stocks %>%
#     subset(ticker %in% stocks_ta_screen$ticker) %>%
#     graph_anchor_scatter(sector)
# )


# gex proof of concept ---------------------------------------------------------
library(tidyquant)
gex <- read.csv('https://squeezemetrics.com/monitor/static/DIX.csv') %>% 
  dplyr::mutate(date = lubridate::as_date(date)) %>% 
  dplyr::select(date, gex) %>% 
  dplyr::filter(gex < 0) %>% 
  dplyr::filter(year(date) >= 2018) # 2014
price <- tidyquant::tq_get("SPY") %>% 
  dplyr::mutate(sma = SMA(close, 200)) %>% 
  dplyr::select(date, close, sma) %>% 
  dplyr::filter(year(date) >= 2018)

p_gex <- gex %>% 
  dplyr::left_join(price) %>% 
  dplyr::mutate(col = ifelse(close > sma, "BULL", "BEAR"))

ggplot() + 
  geom_vline(data = p_gex, aes(xintercept = date, color = col), alpha = 0.3, key_glyph = draw_key_rect) + 
  geom_line(data = price, aes(date, close), size = 1.05) + 
  geom_line(data = price, aes(date, sma), color = "dodgerblue", size = 1.05) +
  scale_color_manual(values = c("tomato", "limegreen")) +
  labs(x = "Date", y = "SPY Price", color = "Market Environment Buy Signal") +
  theme_bw() + 
  theme(
    legend.position = "top",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) 

