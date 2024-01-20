source('www/util_init.R')
source('www/util.R')


# avwap updates ----------------------------------------------------------

file <- "12Tv_-fBoLMc1hcyTWw5jEzd_z3P5Q-uhjHp12j1WlTo"
df <- googlesheets4::read_sheet(file, "Watchlist")

dates <- data.frame(label = "t", date = "2024-01-02")

ndf <- df %>% 
  dplyr::mutate(
    a = purrr::map(Ticker, ~ get_avwap_series(.x, dates))
  ) %>% 
  select(Ticker, Price, a) 

odf <- ndf %>% 
  mutate(avwap = purrr::map_dbl(a, function(d) if(nrow(d) == 0) NA else d[1,1,drop=TRUE])) %>% 
  select(Ticker, avwap)

assertthat::assert_that(nrow(df) == nrow(odf))
googlesheets4::write_sheet(odf, file, 'add_avwaps')


# etf screen -------------------------------------------------------------------
graph_ma_by_group(stocks)

plotly::ggplotly(
  etfs %>% 
    dplyr::filter(type %in% c("major", "factor", "sector")) %>% 
    graph_anchor_scatter(category)
)

spy_1m <- dplyr::filter(etfs, ticker == "SPY")$return_1m
etf_f <- etfs %>% 
  dplyr::filter(return_50d > 0 & return_200d > 0 & return_50d < return_200d) %>% 
  dplyr::filter(return_avwap_anchor_1 >= 0) %>% 
  dplyr::filter(return_1m > spy_1m) %>% 
  dplyr::mutate(rsi = purrr::map(ticker, get_rsi_stats)) %>%
  tidyr::unnest(rsi) %>% 
  dplyr::select(
    ticker, desc, type, category, category2, 
    return_anchor_1, return_avwap_anchor_1,
    return_anchor_2, return_avwap_anchor_2,
    days_since_os, 
    dplyr::matches("52")
  ) %>% 
  dplyr::arrange(dplyr::desc(below_52w_high)) 

etf_f %>% View()


# other stocks ----------------------------------------------------------------
(os <- googlesheets4::read_sheet("10oKFDmMz0UY78I4ARn2JKblJsajIcw2Eda3O-Afqpxs"))
os_clean <- clean_data_stocks(os)
scan2 <- apply_technical_screen(os_clean)

plotly::ggplotly(
  os %>% 
    subset(ticker %in% scan2$ticker) %>% 
    graph_anchor_scatter(sector)
)

plotly::ggplotly(
  stocks %>% 
    subset(ticker %in% stocks_ta_screen$ticker) %>% 
    graph_anchor_scatter(sector)
)


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


# 50d 200d trend ---------------------------------------------------------------
plot_data <- readr::read_csv("data/stats_ma_50d_gr_200d.csv") %>% 
  dplyr::mutate(
    hazy = ifelse(date < lubridate::ymd("2023-03-31"), '1', '2')
  )

plot_data %>% 
  ggplot(aes(date, p, linetype = hazy, color = size)) + 
  geom_hline(yintercept = c(0, 100), color = "grey50") +
  geom_hline(yintercept = 50, color = "grey50", linetype = "dotted") +
  geom_line(size = 0.75) + 
  scale_x_date(date_breaks = "6 months") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  scale_linetype_manual(values = c("dashed", "solid"), name = NULL) +
  scale_color_manual(values = c("black", "blue", "dodgerblue")) +
  labs(x = "", y = "% Stocks where 50MA > 200MA") + 
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )


