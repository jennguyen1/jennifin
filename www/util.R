
## some date stuff ##

today <- Sys.Date()
last_year <- lubridate::year(today)-2


### table creation ###

pdiff <- function(x, base) round((x-base)/base, 3)

calc_rsi_14 <- function(ticker){
  tryCatch({
    tidyquant::tq_get(ticker, from = today - 252) %>% 
    dplyr::mutate(rsi = TTR::RSI(close, n = 14)) %>% 
    tail(1) %>% 
    dplyr::pull(rsi) %>% 
    round(1)
  }, error = function(e) NA
  )
}

calc_sma <- function(ticker, n){
  assertthat::assert_that(n <= 600)
  tidyquant::tq_get(ticker, from = today - 756) %>%
    dplyr::mutate(sma = TTR::SMA(close, n = n)) %>%
    tail(1) %>%
    dplyr::pull(sma)
}

clean_data_etfs <- function(dat){
  dat %>% 
    dplyr::mutate(
      return_1m = pdiff(price, price_1m),
      return_3m = pdiff(price, price_3m),
      return_6m = pdiff(price, price_6m),
      return_200d = pdiff(price, price_200d),
      return_1y = pdiff(price, price_12m),
      return_ytd = pdiff(price, price_year_start),
      return_prev_ytd = pdiff(price, price_prior_year_start),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      rsi14 = purrr::map_dbl(ticker, calc_rsi_14)
    ) %>% 
    dplyr::select(-starts_with("price"))
}

clean_data_stocks <- function(dat){
  dat %>% 
    dplyr::mutate(
      return_200d = pdiff(price, price_200d),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi)
    ) %>% 
    dplyr::select(-starts_with("price"))
}

calculate_perc_above_200d <- function(dat){
  dat %>% 
    dplyr::mutate(above = return_200d >= 0) %>% 
    dplyr::pull(above) %>% 
    mean(na.rm = TRUE)
}

create_display_row <- function(get_ticker, etfs, stocks){
  
  # % sectors above
  what_sector <- switch(
    get_ticker,
    "SPY" = "lrg",
    "RSP" = "ew",
    "IWM" = "small",
    NA
  )
  p_sectors <- etfs %>% 
    dplyr::filter(category2 == what_sector) %>% 
    calculate_perc_above_200d()
  
  # % components above
  if( !stringr::str_detect(get_ticker, "^X") ){
    what_size <- switch(
      get_ticker, 
      "SPY" = "LRG",
      "RSP" = "LRG", 
      "IJH" = "MID",
      "IWM" = "SML", 
      NA
    )
    p_components <- stocks %>% 
      dplyr::filter(size == what_size) %>% 
      calculate_perc_above_200d()
  } else{
    what_s_sector <- switch(
      get_ticker, 
      "XLB" = "Materials",
      "XLE" = "Energy",
      "XLF" = "Financials",
      "XLI" = "Industrials",
      "XLC" = "Communication Services",
      "XLK" = "Information Technology",
      "XLY" = "Consumer Discretionary",
      "XLRE" = "Real Estate",
      "XLP" = "Consumer Staples",
      "XLU" = "Utilities",
      "XLV" = "Health Care",
      NA
    )
    p_components <- stocks %>% 
      dplyr::filter(sector == what_s_sector) %>% 
      calculate_perc_above_200d()
  }

  # add to performance table
  etfs %>% 
    dplyr::filter(ticker == get_ticker) %>% 
    dplyr::mutate(
      ticker = get_ticker,
      p_components = p_components,
      p_sectors = p_sectors
    ) %>% 
    dplyr::select(
      ticker,
      return_200d, dplyr::starts_with("p"), 
      dplyr::starts_with("return"),
      dplyr::matches("52w"), rsi14
    ) 
}


### viz functions ###

filter_stocks <- function(dat, sz, sct){
  d_out <- if( length(sz) == 0 & length(sct) == 0 ){
    dat
  } else if( length(sct) == 0 ){
    dat %>% dplyr::filter(size %in% sz)
  } else if( length(sz) == 0){
    dat %>% dplyr::filter(sector %in% sct)
  } else{
    dat %>% dplyr::filter(size %in% sz & sector %in% sct)
  }
  
  d_out %>% dplyr::pull(ticker)
}

graph_lead_lag <- function(dat, sub = NULL, ...){
  graph_data0 <- if( is.null(sub) | length(sub) == 0 ){
    dat
  } else{
    dplyr::filter(dat, ticker %in% sub)
  }
  
  graph_data <- graph_data0 %>% 
    dplyr::mutate(
      above_52w_low = round(above_52w_low*100, 1),
      below_52w_high = round(below_52w_high*100, 1)
    )
  
  # scales
  xmax <- ceiling( max(graph_data$above_52w_low, na.rm = TRUE) / 100 ) * 100
  ymin <- floor( min(graph_data$below_52w_high, na.rm = TRUE) / 10 ) * 10
  
  # shading region
  fill_green <- data.frame(x = seq(0, xmax, 10)) %>% dplyr::mutate(y = 0 - 0.25*x)
  fill_red <- data.frame(x = seq(0, xmax, 10)) %>% dplyr::mutate(y = 0 - 2*x)
  
  # graph
  g <-   graph_data %>% 
    ggplot() +
    geom_ribbon(data = fill_green, aes(x, ymin = y, ymax = 0), fill = "green", alpha = 0.15) +
    geom_ribbon(data = fill_red, aes(x, ymin = -1000, ymax = y), fill = "red", alpha = 0.15) +
    geom_hline(yintercept = 0, color = "grey80") +
    geom_vline(xintercept = 0, color = "grey80")
  
  g <- if( is.null(col) ){
    g + geom_text(aes(above_52w_low, below_52w_high, label = ticker)) 
  } else{
    g + geom_text(aes(above_52w_low, below_52w_high, label = ticker, ...))
  }
  
  g +
    scale_x_continuous(expand = expansion(add = c(1,0))) +
    scale_y_continuous(expand = expansion(add = c(0,1)))  +
    coord_cartesian(xlim = c(0, xmax), ylim = c(ymin, 0)) +
    labs(x = "Above 52W Low %", y = "Below 52W High %", color = "Category") +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
}

tabulate_performance_etfs <- function(dat, sub = NULL){
  tab_data0 <- if( is.null(sub) | length(sub) == 0 ){
    dat
  } else{
    dplyr::filter(dat, ticker %in% sub)
  }
  
  tab_data <- tab_data0 %>% 
    dplyr::select(-type, -dplyr::starts_with("category"))
  
  # tabulate
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker", 
      "Description" = "desc",
      "1M %" = "return_1m",
      "3M %" = "return_3m",
      "6M %" = "return_6m",
      "200D %" = "return_200d",
      "1Y %" = "return_1y", 
      "YTD %" = "return_ytd", 
      "Prior YTD %" = "return_prev_ytd",
      'Above 52W Low %' = 'above_52w_low',
      "Below 52W High %" = "below_52w_high",
      "RSI" = "rsi14"
    ),  
    class = 'cell-border compact hover',
    filter = 'bottom',
    select = 'none',
    options = list(
      dom = 'tr', # table display
      order = list(list(10, 'desc')), # default order based on 52w high
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = min(375, nrow(tab_data)*30)
    )
  ) %>% 
    # formatting
    formatPercentage(3:11, digits = 1) %>% 
    formatStyle(1, fontWeight = "bold") %>% 
    formatStyle(3:11, color = styleInterval(0, c("red", "green"))) %>% 
    formatStyle(3:11, color = styleEqual(0, "black")) %>% 
    formatStyle(12, color = styleInterval(c(30, 70), c("orange", "black", "orange")))
}

tabulate_performance_stocks <- function(dat, sub = NULL){
  tab_data0 <- if( is.null(sub) | length(sub) == 0 ){
    dat
  } else{
    dplyr::filter(dat, ticker %in% sub)
  }
  
  tab_data <- tab_data0 %>% 
    dplyr::mutate_at(dplyr::vars(sector, size), factor)
  
  # tabulate
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker", 
      "Company" = "company",
      "Sector" = "sector", 
      "Size" = "size",
      "200D %" = "return_200d",
      'Above 52W Low %' = 'above_52w_low',
      "Below 52W High %" = "below_52w_high"
    ),  
    class = 'cell-border compact hover',
    filter = list(
      position = 'top',
      clear = FALSE
    ),
    select = 'multiple', 
    options = list(
      dom = 'tr', # table display
      order = list(list(6, 'desc')), # default order based on 52w high
      columnDefs = list(list(className = 'dt-center', targets = 3)),
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = 375
    )
  ) %>% 
    # formatting
    formatPercentage(5:7, digits = 1) %>% 
    formatStyle(1, fontWeight = "bold") %>% 
    formatStyle(5:7, color = styleInterval(0, c("red", "green"))) %>% 
    formatStyle(3:11, color = styleEqual(0, "black"))
}

display_table_summary <- function(etfs, stocks){
  
  tab_data <- purrr::map_dfr(
    c("SPY", "RSP", "IJH", "IWM", "XLB", "XLE", "XLF", "XLI", "XLC", "XLK", "XLY", "XLRE", "XLP", "XLU", "XLV"), 
    create_display_row, etfs, stocks
  ) %>% 
    dplyr::mutate(rank = c(1:4, rep(5, 11))) %>% 
    dplyr::arrange(rank, dplyr::desc(return_200d)) %>% 
    dplyr::select(-rank)
  
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker",
      "% Components Above 200D" = "p_components",
      "% Sectors Above 200D" = "p_sectors",
      "200D %" = "return_200d",
      "1M %" = "return_1m",
      "3M %" = "return_3m",
      "6M %" = "return_6m",
      "1Y %" = "return_1y", 
      "YTD %" = "return_ytd", 
      "Prior YTD %" = "return_prev_ytd",
      'Above 52W Low %' = 'above_52w_low',
      "Below 52W High %" = "below_52w_high",
      "RSI" = "rsi14"
    ),  
    class = 'cell-border compact hover',
    select = 'none',
    options = list(
      dom = 'tr', # table display
      columnDefs = list(list(className = 'dt-center', targets = 0:12)),
      pageLength = 15,
      scrollX = TRUE, scrollY = 400
    )
  ) %>% 
    # formatting
    formatPercentage(2:12, digits = 1) %>% 
    formatStyle(1, fontWeight = "bold") %>% 
    formatStyle(c(2, 5:12), color = styleInterval(0, c("red", "green"))) %>% 
    formatStyle(c(2, 5:12), color = styleEqual(0, "black")) %>% 
    formatStyle(13, color = styleInterval(c(30, 70), c("orange", "black", "orange")))
}

