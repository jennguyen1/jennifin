
# etf screen -------------------------------------------------------------------
spy_1m <- dplyr::filter(etfs, ticker == "SPY")$return_1m

etfs %>% 
  dplyr::filter(return_200d > 0) %>%  
  dplyr::filter(return_anchor_1 >= 0) %>% 
  dplyr::filter(return_1m > spy_1m) %>% 
  dplyr::mutate(
    sma200_slope = purrr::map_dbl(ticker, get_sma_slope),
    rsi = purrr::map(ticker, get_rsi_stats)
  ) %>% 
  tidyr::unnest(rsi) %>% 
  dplyr::filter(sma200_slope > 0) %>% # keep if 200d SMA slope positive over past 2 weeks
  dplyr::filter(days_since_os > 21*3) %>% # remove if oversold in the last 3m
  dplyr::select(ticker, desc, type, category, category2, return_200d, dplyr::matches("52"), days_since_os) %>% 
  dplyr::arrange(dplyr::desc(below_52w_high)) %>% View()


# international ----------------------------------------------------------------
(id <- googlesheets4::read_sheet("1ClO1wnq-02ImJYbiLV1H3nnjDXWMP5mjN0TuTcOqDRE"))

id %>% 
  dplyr::mutate(
    return_200d = pdiff(price, price_200d),
    above_52w_low = pdiff(price, price_52w_lo),
    below_52w_high = pdiff(price, price_52w_hi),
    return_1m = pdiff(price, price_1m),
    return_anchor_lo = pdiff(price, price_anchor_lo),
    return_anchor_hi = pdiff(price, price_anchor_hi)
  ) %>%  
  dplyr::filter(return_200d > 0) %>%  
  dplyr::filter(return_anchor_hi >= 0) %>% 
  dplyr::filter(return_1m > spy_1m) %>% 
  dplyr::mutate(
    sma200_slope = purrr::map_dbl(ticker, get_sma_slope),
    rsi = purrr::map(ticker, get_rsi_stats)
  ) %>% 
  tidyr::unnest(rsi) %>% 
  dplyr::filter(sma200_slope > 0) %>% # keep if 200d SMA slope positive over past 2 weeks
  dplyr::filter(days_since_os > 21*3) %>% # remove if oversold in the last 3m
  dplyr::select(ticker, dplyr::one_of(c("company", "sector", "size")), return_200d, dplyr::matches("52"), days_since_os) %>% 
  dplyr::arrange(dplyr::desc(below_52w_high)) %>% View()


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
# 50d 200d todo add sep?
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
