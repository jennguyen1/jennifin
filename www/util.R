
## some date stuff ##

today <- Sys.Date()
last_year <- lubridate::year(today)-2
anchor_msg = "Feb High" # note this may change


### general functions ###
pdiff <- function(x, base) round((x-base)/base, 3)

calc_rsi_14 <- function(ticker, i = 1:252){
  use_ticker <- plyr::mapvalues(
    ticker, # ticker transcription if needed
    c("BRKB", "MOG.A"), 
    c("BRK-B", "MOG-A"), 
    warn_missing = FALSE
  )
  
  tryCatch({
    tidyquant::tq_get(use_ticker, from = today - 252*2) %>% 
      dplyr::mutate(rsi = TTR::RSI(close, n = 14)) %>% 
      dplyr::arrange(dplyr::desc(date)) %>% 
      dplyr::select(date, rsi) %>% 
      head(252) %>% 
      dplyr::slice(i) %>% 
      dplyr::pull(rsi) 
  }, error = function(e) NA
  )
}

get_rsi_stats <- function(ticker){
  rsi <- calc_rsi_14(ticker)
  data.frame(
    rsi = rsi[1],
    days_since_os = dplyr::coalesce(which(rsi < 30)[1] - 1, 252),
    days_since_ob = dplyr::coalesce(which(rsi > 70)[1] - 1, 252)
  )
}

calc_sma <- function(ticker, n){
  assertthat::assert_that(n <= 600)
  tidyquant::tq_get(ticker, from = today - 756) %>%
    dplyr::mutate(sma = TTR::SMA(close, n = n)) %>%
    tail(1) %>%
    dplyr::pull(sma)
}


### table creation ###

clean_data_etfs <- function(dat){
  dat %>% 
    dplyr::mutate(
      return_1m = pdiff(price, price_1m),
      return_3m = pdiff(price, price_3m),
      return_6m = pdiff(price, price_6m),
      return_1y = pdiff(price, price_12m),
      return_ytd = pdiff(price, price_year_start),
      return_200d = pdiff(price, price_200d),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      return_anchor = pdiff(price, price_anchor)
      # rsi14 = purrr::map_dbl(ticker, calc_rsi_14) # dont need this
    ) %>% 
    dplyr::select(-starts_with("price"))
}

clean_data_stocks <- function(dat){
  dat %>% 
    dplyr::mutate(
      return_50d = pdiff(price, price_50d),
      return_100d = pdiff(price, price_100d),
      return_200d = pdiff(price, price_200d),
      above_52w_low = pdiff(price, price_52w_lo),
      below_52w_high = pdiff(price, price_52w_hi),
      return_1m = pdiff(price, price_1m),
      return_anchor = pdiff(price, price_anchor)
    ) %>% 
    dplyr::select(-starts_with("price"))
}

calculate_perc_above <- function(dat, val){
  dat %>% 
    dplyr::select(x = dplyr::matches(val)) %>% 
    dplyr::mutate(above = x >= 0) %>% 
    dplyr::pull(above) %>% 
    mean(na.rm = TRUE)
}

create_display_row <- function(get_ticker, etfs, stocks){
  
  # % components above
  p_components_0 <- if( !stringr::str_detect(get_ticker, "^X") ){
    what_size <- switch(
      get_ticker, 
      "SPY" = "LRG",
      "RSP" = "LRG", 
      "IJH" = "MID",
      "IWM" = "SML", 
      NA
    )
    dplyr::filter(stocks, size == what_size)
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
    dplyr::filter(stocks, sector == what_s_sector & size == "LRG")
  }
  
  # add to performance table
  etfs %>% 
    dplyr::filter(ticker == get_ticker) %>% 
    dplyr::mutate(
      ticker = get_ticker,
      p_components_50d = calculate_perc_above(p_components_0, "50d"),
      p_components_100d = calculate_perc_above(p_components_0, "100d"),
      p_components_200d = calculate_perc_above(p_components_0, "200d"),
      p_components_anchor = calculate_perc_above(p_components_0, "anchor")
    ) %>% 
    dplyr::select(
      ticker,
      dplyr::matches("components_\\d+d"), return_200d, 
      p_components_anchor, return_anchor
    ) %>% 
    dplyr::rename_with(~ plyr::mapvalues(., 
                         c("p_components_anchor", "return_anchor"), 
                         c(paste("% Above", anchor_msg), paste(anchor_msg, "%")))
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
    dplyr::select(
      ticker, desc, 
      dplyr::matches("\\d[my]$"), dplyr::ends_with("ytd"),
      dplyr::ends_with("200d"), dplyr::matches("52w")
    )

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
      "1Y %" = "return_1y", 
      "YTD %" = "return_ytd", 
      "200D %" = "return_200d",
      'Above 52W Low %' = 'above_52w_low',
      "Below 52W High %" = "below_52w_high"
    ),  
    class = 'cell-border compact hover',
    filter = 'bottom',
    select = 'none',
    options = list(
      dom = 'tr', # table display
      order = list(list(ncol(tab_data)-1, 'desc')), # default order based on 52w high
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = min(375, nrow(tab_data)*30)
    )
  ) %>% 
    # formatting
    DT::formatPercentage(3:ncol(tab_data), digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(3:ncol(tab_data), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(3:ncol(tab_data), color = DT::styleEqual(0, "black")) 
}

tabulate_performance_stocks <- function(dat, sub = NULL){
  tab_data0 <- if( is.null(sub) | length(sub) == 0 ){
    dat
  } else{
    dplyr::filter(dat, ticker %in% sub)
  }
  
  tab_data <- tab_data0 %>% 
    dplyr::mutate_at(dplyr::vars(sector, size), factor) %>% 
    dplyr::select(ticker, company, sector, size, return_200d, dplyr::matches("52w"))

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
    filter = list(position = 'top', clear = FALSE),
    select = 'none', 
    extensions = "Buttons",
    options = list(
      dom = 'Btr', # table display
      buttons = list(list( # export list
          extend = "csv", text = "Download",
          filename = paste0("stock_list_", stringr::str_replace_all(Sys.Date(), "-", "_"))
      )),
      order = list(list(6, 'desc')), # default order based on 52w high
      columnDefs = list(list(className = 'dt-center', targets = 3)),
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = 370
    )
  ) %>% 
    # formatting
    DT::formatPercentage(5:ncol(tab_data), digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(5:ncol(tab_data), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(5:ncol(tab_data), color = DT::styleEqual(0, "black"))
}

display_table_summary <- function(etfs, stocks){
  
  tab_data <- purrr::map_dfr(
    c("SPY", "IJH", "IWM", "XLF", "XLI", "XLB", "XLE", "XLY", "XLK", "XLC", "XLRE", "XLP", "XLU", "XLV"), 
    create_display_row, etfs, stocks
  )
  
  anchor_cut <- quantile(tab_data[, 6, drop = TRUE], c(0.25, 0.75))
  
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker",
      "% Above 50D" = "p_components_50d",
      "% Above 100D" = "p_components_100d",
      "% Above 200D" = "p_components_200d",
      "200D %" = "return_200d"
    ),
    class = 'cell-border compact hover',
    select = 'none',
    options = list(
      dom = 'tr', # table display
      columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tab_data)-1))),
      pageLength = 15,
      scrollX = TRUE, scrollY = 410
    )
  ) %>% 
    # formatting
    DT::formatPercentage(2:7, digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(c(5, 7), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(c(5, 7), color = DT::styleEqual(0, "black")) %>% 
    DT::formatStyle(c(2:4), backgroundColor = DT::styleInterval(c(1/3, 0.4999, 2/3), c(rgb(1,0,0,.15), rgb(1, 1, 0, 0.2), "white", rgb(0,1,0,.15)))) %>% 
    DT::formatStyle(6, backgroundColor = DT::styleInterval(anchor_cut, c(rgb(1,0,0,.15), "white", rgb(0,1,0,.15))))
}

