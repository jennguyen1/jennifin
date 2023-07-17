
## some date stuff ##

anchor_lo_msg <- "Oct Low" # note this may change
anchor_hi_msg <- "Feb High"


### general functions ###
calculate_perc_above <- function(dat, val){
  tryCatch({
    dat %>% 
      dplyr::select(x = dplyr::matches(val)) %>% 
      dplyr::mutate(above = x >= 0) %>% 
      dplyr::pull(above) %>% 
      mean(na.rm = TRUE)
  }, error = function(e){
    NA
  })
}


### table creation ###

create_display_row <- function(get_ticker, etfs, stocks){
  
  # % components above
  p_components_0 <- if( !stringr::str_detect(get_ticker, "^X|^E") ){
    what_size <- switch(
      get_ticker, 
      "SPY" = "LRG",
      "RSP" = "LRG", 
      "IJH" = "MID",
      "IJR" = "SML", 
      NA
    )
    dplyr::filter(stocks, size == what_size)
  } else if( !stringr::str_detect(get_ticker, "^X") ){
    what_category <- switch(
      get_ticker, 
      "EFA" = "developed", 
      "EEM" = "emerging", 
      NA
    )
    dplyr::filter(etfs, category2 == what_category) %>% dplyr::select(-slope_200d)
  } else{
    what_sector <- switch(
      get_ticker, 
      "XLB" = "Materials",
      "XLE" = "Energy",
      "XLF" = "Financial",
      "XLI" = "Industrial",
      "XLC" = "Communication Services",
      "XLK" = "Technology",
      "XLY" = "Consumer Discretionary",
      "XLRE" = "Real Estate",
      "XLP" = "Consumer Staples",
      "XLU" = "Utilities",
      "XLV" = "Health Care",
      NA
    )
    dplyr::filter(stocks, sector == what_sector & size == "LRG")
  }
  
  # add to performance table
  etfs %>% 
    dplyr::filter(ticker == get_ticker) %>% 
    dplyr::mutate(
      ticker = get_ticker,
      p_components_20d = calculate_perc_above(p_components_0, "20d"),
      p_components_50d = calculate_perc_above(p_components_0, "50d"),
      p_components_200d = calculate_perc_above(p_components_0, "200d"),
      ma_50d_200d = ifelse(return_50d < return_200d, "Yes", "No"), 
      p_components_anchor_lo = calculate_perc_above(p_components_0, "anchor_lo"),
      p_components_anchor_hi = calculate_perc_above(p_components_0, "anchor_hi")
    ) %>% 
    dplyr::select(
      ticker,
      dplyr::matches("components_\\d+d"), 
      return_50d, slope_50d,
      return_200d, slope_200d,
      ma_50d_200d,
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
      dplyr::ends_with("50d"), dplyr::ends_with("200d"), 
      dplyr::matches("52w")
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
      "50D %" = "return_50d",
      "50D Slope" = "slope_50d",
      "200D %" = "return_200d",
      "200D Slope" = "slope_200d",
      'Above 52W Low' = 'above_52w_low',
      "Below 52W High" = "below_52w_high"
    ),  
    class = 'cell-border compact hover',
    filter = 'bottom',
    select = 'none',
    options = list(
      dom = 'tr', # table display
      columnDefs = list(list(className = 'dt-center', targets = c(8, 10))),
      order = list(list(ncol(tab_data)-1, 'desc')), # default order based on 52w high
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = min(375, nrow(tab_data)*30)
    )
  ) %>% 
    # formatting
    DT::formatPercentage(c(3:8, 10, 12:ncol(tab_data)), digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(c(3:8, 10, 12:ncol(tab_data)), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(c(3:8, 10, 12:ncol(tab_data)), color = DT::styleEqual(0, "black")) %>% 
    DT::formatStyle(c(9, 11), color = DT::styleEqual(c("-", "0", "+"), c("red", "black", "green")))
}

tabulate_performance_stocks <- function(dat, sub = NULL){
  tab_data0 <- if( is.null(sub) | length(sub) == 0 ){
    dat
  } else{
    dplyr::filter(dat, ticker %in% sub)
  }
  
  tab_data <- tab_data0 %>% 
    dplyr::mutate_at(dplyr::vars(sector, industry, size), factor) %>% 
    dplyr::select(ticker, company, sector, industry, size, return_200d, dplyr::matches("52w"), `Days Since OS` = dplyr::matches("days_since_os")) 

  # tabulate
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker", 
      "Company" = "company",
      "Sector" = "sector", 
      "Industry" = "industry",
      "Size" = "size",
      "200D %" = "return_200d",
      'Above 52W Low' = 'above_52w_low',
      "Below 52W High" = "below_52w_high"
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
      order = list(list(7, 'desc')), # default order based on 52w high
      columnDefs = list(list(className = 'dt-center', targets = 4)),
      pageLength = nrow(tab_data), # minimal scrolling
      scrollX = TRUE, scrollY = 341
    )
  ) %>% 
    # formatting
    DT::formatPercentage(6:8, digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(6:8, color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(6:8, color = DT::styleEqual(0, "black"))
}

display_table_summary <- function(etfs, stocks){
  
  tab_data <- purrr::map_dfr(
    c("SPY", "IJH", "IJR", "XLF", "XLI", "XLB", "XLE", "XLY", "XLK", "XLC", "XLRE", "XLP", "XLU", "XLV", "EFA", "EEM"), 
    create_display_row, etfs, stocks
  )
  
  anchor_lo_cut <- quantile(tab_data[1:14, 10, drop = TRUE], c(0.25, 0.75))
  anchor_hi_cut <- quantile(tab_data[1:14, 12, drop = TRUE], c(0.25, 0.75))
  
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker",
      "% Above 20D" = "p_components_20d",
      "% Above 50D" = "p_components_50d",
      "% Above 200D" = "p_components_200d",
      "50D %" = "return_50d",
      "50D Slope" = "slope_50d",
      "200D %" = "return_200d",
      "200D Slope" = "slope_200d",
      "50D > 200D" = "ma_50d_200d"
    ),
    class = 'cell-border compact hover',
    select = 'none',
    options = list(
      dom = 'tr', # table display
      columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(tab_data)-1))),
      pageLength = 16,
      scrollX = TRUE, scrollY = 465
    )
  ) %>% 
    # formatting
    DT::formatPercentage(c(2:4, 10, 12), digits = 0) %>% 
    DT::formatPercentage(c(5, 7, 11, 13), digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(c(5, 7, 11, 13), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(c(5, 7, 11, 13), color = DT::styleEqual(0, "black")) %>% 
    DT::formatStyle(c(6, 8), color = DT::styleEqual(c("-", "0", "+"), c("red", "black", "green"))) %>% 
    DT::formatStyle(c(9), color = DT::styleEqual(c("No", "Yes"), c("red", "green"))) %>% 
    DT::formatStyle(c(2:4), backgroundColor = DT::styleInterval(c(1/3, 0.4999, 2/3), c(rgb(1,0,0,.15), rgb(1, 1, 0, 0.2), "white", rgb(0,1,0,.15)))) %>% 
    DT::formatStyle(10, backgroundColor = DT::styleInterval(anchor_lo_cut, c(rgb(1,0,0,.15), "white", rgb(0,1,0,.15)))) %>% 
    DT::formatStyle(12, backgroundColor = DT::styleInterval(anchor_hi_cut, c(rgb(1,0,0,.15), "white", rgb(0,1,0,.15))))
}

