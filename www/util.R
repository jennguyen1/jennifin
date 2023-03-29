
## some date stuff ##

today <- Sys.Date()
last_year <- lubridate::year(today)-2
anchor_lo_msg <- "Oct Low" # note this may change
anchor_hi_msg <- "Feb High"


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
    rsi2 = rsi[10], # todo
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

calculate_perc_above <- function(dat, val){
  dat %>% 
    dplyr::select(x = dplyr::matches(val)) %>% 
    dplyr::mutate(above = x >= 0) %>% 
    dplyr::pull(above) %>% 
    mean(na.rm = TRUE)
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
      return_anchor_lo = pdiff(price, price_anchor_lo),
      return_anchor_hi = pdiff(price, price_anchor_hi)
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
      return_anchor_lo = pdiff(price, price_anchor_lo),
      return_anchor_hi = pdiff(price, price_anchor_hi)
    ) %>% 
    dplyr::select(-starts_with("price"))
}

apply_technical_screen <- function(dat, etfs){
  spy_1m <- dplyr::filter(etfs, ticker == "SPY")$return_1m
  
  # initial filter based 200D, S/R & RS to spy
  d1 <- dat %>% 
    dplyr::filter(return_200d > 0) %>%  # keep above 200d SMA
    dplyr::filter(return_anchor_hi >= -0.05) %>% # no more than 5% below anchor DATE high (target something like 52w high for SP1500)
    dplyr::filter(return_1m > spy_1m)  # remove laggards to SPY over last 1m
  
  # filter those in bullish rsi regime
  d2 <- d1 %>% 
    dplyr::mutate(rsi = purrr::map(ticker, get_rsi_stats)) %>% 
    tidyr::unnest(rsi) %>% 
    dplyr::filter(days_since_os > 21*3) %>% # remove if oversold in the last 3m
    dplyr::select(ticker, dplyr::one_of(c("company", "sector", "size")), return_200d, dplyr::matches("52"), days_since_os) 
  
  # sort by 52w highs
  d2 %>% dplyr::arrange(dplyr::desc(below_52w_high))
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
      p_components_anchor_lo = calculate_perc_above(p_components_0, "anchor_lo"),
      p_components_anchor_hi = calculate_perc_above(p_components_0, "anchor_hi")
    ) %>% 
    dplyr::select(
      ticker,
      dplyr::matches("components_\\d+d"), return_200d, 
      p_components_anchor_lo, return_anchor_lo, 
      p_components_anchor_hi, return_anchor_hi
    ) %>% 
    dplyr::rename_with(~ plyr::mapvalues(., 
                         c("p_components_anchor_lo", "return_anchor_lo", "p_components_anchor_hi", "return_anchor_hi"), 
                         c(paste("% Above", anchor_lo_msg), paste(anchor_lo_msg, "%"), paste("% Above", anchor_hi_msg), paste(anchor_hi_msg, "%")))
    )
}


### viz functions ###

filter_stocks <- function(dat, sz, sct, ta_scn, ta_lst){
  # size & sector screen
  d_out <- if( length(sz) == 0 & length(sct) == 0 ){
    dat
  } else if( length(sct) == 0 ){
    dat %>% dplyr::filter(size %in% sz)
  } else if( length(sz) == 0){
    dat %>% dplyr::filter(sector %in% sct)
  } else{
    dat %>% dplyr::filter(size %in% sz & sector %in% sct)
  }
  
  # ta list screen
  if(!is.null(ta_scn)){
    if(ta_scn){
      d_out <- d_out %>% dplyr::filter(ticker %in% ta_lst)
    }
  }

  d_out$ticker
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
  xmax <- ceiling( max(graph_data$above_52w_low, na.rm = TRUE) / 10 ) * 10 + 10
  ymin <- floor( min(graph_data$below_52w_high, na.rm = TRUE) / 10 ) * 10 - 5
  
  # shading region
  fill_green <- data.frame(x = seq(0, xmax, 5)) %>% dplyr::mutate(y = 0 - 0.25*x)
  fill_red <- data.frame(x = seq(0, xmax, 5)) %>% dplyr::mutate(y = 0 - 2*x)
  
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
    coord_cartesian(xlim = c(-1, xmax), ylim = c(ymin, 1)) +
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
    dplyr::select(ticker, company, sector, size, return_200d, dplyr::matches("52w"), `Days Since OS` = dplyr::matches("days_since_os")) 

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
      dom = 'Btri', # table display 
      buttons = list(list( # export list
          extend = "csv", text = "Download",
          filename = paste0("stock_list_", stringr::str_replace_all(Sys.Date(), "-", "_"))
      )),
      order = list(list(6, 'desc')), # default order based on 52w high
      columnDefs = list(list(className = 'dt-center', targets = 3)),
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = 341
    )
  ) %>% 
    # formatting
    DT::formatPercentage(5:7, digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(5:7, color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(5:7, color = DT::styleEqual(0, "black"))
}

display_table_summary <- function(etfs, stocks){
  
  tab_data <- purrr::map_dfr(
    c("SPY", "IJH", "IWM", "XLF", "XLI", "XLB", "XLE", "XLY", "XLK", "XLC", "XLRE", "XLP", "XLU", "XLV"), 
    create_display_row, etfs, stocks
  )
  
  anchor_lo_cut <- quantile(tab_data[, 6, drop = TRUE], c(0.25, 0.75))
  anchor_hi_cut <- quantile(tab_data[, 8, drop = TRUE], c(0.25, 0.75))
  
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
      scrollX = TRUE, scrollY = 407
    )
  ) %>% 
    # formatting
    DT::formatPercentage(2:9, digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(c(5, 7, 9), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(c(5, 7, 9), color = DT::styleEqual(0, "black")) %>% 
    DT::formatStyle(c(2:4), backgroundColor = DT::styleInterval(c(1/3, 0.4999, 2/3), c(rgb(1,0,0,.15), rgb(1, 1, 0, 0.2), "white", rgb(0,1,0,.15)))) %>% 
    DT::formatStyle(6, backgroundColor = DT::styleInterval(anchor_lo_cut, c(rgb(1,0,0,.15), "white", rgb(0,1,0,.15)))) %>% 
    DT::formatStyle(8, backgroundColor = DT::styleInterval(anchor_hi_cut, c(rgb(1,0,0,.15), "white", rgb(0,1,0,.15))))
}

