
source('www/util_init.R')

### general functions ###
calculate_perc_above <- function(dat, val){
  tryCatch({
    dat %>% 
      dplyr::select(x = dplyr::one_of(val)) %>% 
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
    dplyr::filter(etfs, category2 == what_category) 
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
      p_components_20d = calculate_perc_above(p_components_0, "return_20d"),
      p_components_50d = calculate_perc_above(p_components_0, "return_50d"),
      p_components_200d = calculate_perc_above(p_components_0, "return_200d"),
      ma_50d_200d = ifelse(return_50d < return_200d, "Yes", "No"), 
      p_components_anchor_1 = calculate_perc_above(p_components_0, "return_anchor_1"),
      p_components_anchor_2 = calculate_perc_above(p_components_0, "return_anchor_2"),
      p_components_avwap_anchor_1 = calculate_perc_above(p_components_0, "return_avwap_anchor_1"),
      p_components_avwap_anchor_2 = calculate_perc_above(p_components_0, "return_avwap_anchor_2")
    ) %>% 
    dplyr::select(
      ticker,
      dplyr::matches("components_\\d+d"), 
      return_50d, return_200d, ma_50d_200d,
      return_avwap_ytd, 
      p_components_avwap_anchor_1, return_avwap_anchor_1, p_components_anchor_1, return_anchor_1,
      p_components_avwap_anchor_2, return_avwap_anchor_2, p_components_anchor_2, return_anchor_2
    ) %>% 
    dplyr::rename_with(~ plyr::mapvalues(., 
                         c("p_components_anchor_1", "return_anchor_1", "p_components_avwap_anchor_1", "return_avwap_anchor_1", 
                           "p_components_anchor_2", "return_anchor_2", "p_components_avwap_anchor_2", "return_avwap_anchor_2"), 
                         c(paste("% Above", anchor_1_msg), paste(anchor_1_msg, "%"), paste("% Above AVWAP", anchor_1_msg), paste(anchor_1_msg, "AVWAP %"), 
                           paste("% Above", anchor_2_msg), paste(anchor_2_msg, "%"), paste("% Above AVWAP", anchor_2_msg), paste(anchor_2_msg, "AVWAP %")))
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
      return_above_52w_lo = round(return_above_52w_lo*100, 1),
      return_below_52w_hi = round(return_below_52w_hi*100, 1)
    )
  
  # scales
  xmax <- ceiling( max(graph_data$return_above_52w_lo, na.rm = TRUE) / 10 ) * 10 + 10
  ymin <- floor( min(graph_data$return_below_52w_hi, na.rm = TRUE) / 10 ) * 10 - 5
  
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
    g + geom_text(aes(return_above_52w_lo, return_below_52w_hi, label = ticker)) 
  } else{
    g + geom_text(aes(return_above_52w_lo, return_below_52w_hi, label = ticker, ...))
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
      dplyr::matches("\\d+m$"), return_ytd, return_avwap_ytd,
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
      "1Y %" = "return_12m", 
      "YTD %" = "return_ytd", 
      "YTD AVWAP %" = "return_avwap_ytd",
      "50D %" = "return_50d",
      "200D %" = "return_200d",
      'Above 52W Low' = 'return_above_52w_lo',
      "Below 52W High" = "return_below_52w_hi"
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
    DT::formatPercentage(c(3:ncol(tab_data)), digits = 1) %>% 
    DT::formatStyle(1, fontWeight = "bold") %>% 
    DT::formatStyle(c(3:ncol(tab_data)), color = DT::styleInterval(0, c("red", "green"))) %>% 
    DT::formatStyle(c(3:ncol(tab_data)), color = DT::styleEqual(0, "black")) 
}

display_table_summary <- function(etfs, stocks){
  
  tab_data <- purrr::map_dfr( 
    c("SPY", "IJH", "IJR", "XLF", "XLI", "XLB", "XLE", "XLY", "XLK", "XLC", "XLRE", "XLP", "XLU", "XLV", "EFA", "EEM"), 
    create_display_row, etfs, stocks
  ) %>% 
    dplyr::select(ticker, return_50d, return_200d, ma_50d_200d, return_avwap_ytd, dplyr::ends_with("%"))
  
  DT::datatable(
    tab_data,
    rownames = FALSE,
    colnames = c(
      "Ticker" = "ticker",
      "50D %" = "return_50d",
      "200D %" = "return_200d",
      "50D > 200D" = "ma_50d_200d",
      "YTD AVWAP %" = "return_avwap_ytd"
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
    DT::formatPercentage(c(2:3, 5:9), digits = 1) %>%
    DT::formatStyle(1, fontWeight = "bold") %>%
    DT::formatStyle(c(2:3, 5:9), color = DT::styleInterval(0, c("red", "green"))) %>%
    DT::formatStyle(c(2:3, 5:9), color = DT::styleEqual(0, "black")) %>%
    DT::formatStyle(c(4), backgroundColor = DT::styleEqual(c("No", "Yes"), c(rgb(1,0,0,.15), rgb(0,1,0,.15)))) 
}

# breadth by sector
graph_ma_uptrend_by_group <- function(past_years = 2){
  yr <- year(today) - past_years # todo
  
  df <- query_db(stringr::str_glue("
  SELECT 
    s.ticker, s.company, s.sector, s.industry, s.size, sectors.category,
    p.date, p.open, p.high, p.low, p.close, p.volume, p.rsi, p.price_20d, p.price_50d, p.price_200d
  FROM stocks as s
  LEFT JOIN price_stats as p
    ON s.ticker = p.ticker
  LEFT JOIN sectors
    ON s.sector = sectors.sector
  WHERE date > '{yr}-01-01'
"))
  
  sdf <- df %>% # by size
    dplyr::group_by(date, size) %>% 
    dplyr::summarise(
        p = mean(close > price_50d & close > price_200d & price_50d > price_200d, na.rm = TRUE)
      ) %>% 
    dplyr::mutate(type = "By Size", category = size)

  adf <- df %>% # by sector
    dplyr::group_by(date, category) %>% 
    dplyr::summarise(
      p = mean(close > price_50d & close > price_200d & price_50d > price_200d, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(type = "By Group")

  # graphing
  plot_data <- dplyr::bind_rows(sdf, adf) %>% 
    dplyr::mutate( 
      type = factor(type, levels = c("By Size", "By Group")),
      category = stringr::str_to_title(category),
      category = factor(category, levels = c("Lrg", "Mid", "Sml", "Cyclical", "Growth", "Defensive"))
    )
  
  plot_lab <- plot_data %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(date == max(date))
  
  plot_data %>% 
    ggplot(aes(date, p*100, color = category)) +
    geom_line(linewidth = 0.9) +
    geom_text(
      data = plot_lab, 
      aes(date, p*100, label = category, color = category),
      hjust = -0.15
    ) +
    geom_hline(yintercept = c(0, 100), color = "grey50") +
    geom_hline(yintercept = 50, color = "grey50", linetype = "dashed") +
    facet_grid(~type) +
    expand_limits(x = as.Date(today + 90)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_color_manual(values = c("black", "grey30", "grey60", "limegreen", "dodgerblue", "tomato")) +
    labs(
      x = "Date", 
      y = "% in Uptrend", 
      caption = "(1) > 50DMA, (2) >200DMA, (3) 50DMA > 200DMA"
    ) +
    theme(
      legend.position = "none", 
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank()
    )
}

graph_ma_by_sector <- function(dat){
  grp <- dat %>% 
    dplyr::distinct(sector) %>% 
    dplyr::arrange(sector) %>% 
    dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "defensive", "growth", "defensive"))
  
  df <- dat %>% 
    dplyr::left_join(grp, "sector") %>% 
    dplyr::select(ticker, sector, group, size, ends_with("d")) %>% 
    dplyr::mutate(dplyr::across(dplyr::ends_with("d"), \(x) x > 0))
  
  sdf <- df %>% # by size
    dplyr::group_by(size) %>% 
    dplyr::summarise(across(
      dplyr::matches("\\dd$"),
      \(x) mean(x, na.rm = TRUE)
    )) %>% 
    dplyr::mutate(group = "all") %>% 
    dplyr::rename(sector = size)
  
  adf <- df %>% # by sector
    dplyr::group_by(group, sector) %>% 
    dplyr::summarise(dplyr::across(
      dplyr::matches("\\dd$"),
      \(x) mean(x, na.rm = TRUE)
    ))
  
  plot_df <- dplyr::bind_rows(sdf, adf) %>% 
    tidyr::pivot_longer(-c(group, sector)) %>% 
    dplyr::mutate(
      days = factor(readr::parse_number(name)),
      sector = plyr::mapvalues(
        sector, 
        c("Consumer Staples", "Communication Services", "Consumer Discretionary"), 
        c("Staples", "Communications", "Discretionary")
      ),
      group = factor(stringr::str_to_title(group), levels = c("All", "Growth", "Cyclical", "Defensive"))
    ) 
  
  plot_df %>% 
    ggplot() +
    geom_hline(yintercept = c(0, 100), color = "grey50") +
    geom_hline(yintercept = 50, color = "grey50", linetype = "dashed") +
    geom_line(aes(days, value*100, group = sector, color = sector), linewidth = 1.25) +
    ggrepel::geom_text_repel(
      data = plot_df %>% dplyr::filter(days == 200),
      aes(3.1, value*100, label = sector, color = sector),
      hjust = 0, direction = "y", segment.color = NA
    ) +
    facet_grid(~group) + 
    scale_y_continuous(breaks = seq(0, 100, 10)) + 
    scale_color_manual(values = c(
      "deepskyblue", "dodgerblue", "darkgreen", "yellowgreen", "red", "green3", "black", 
      "green4", "grey30", "hotpink1", "grey60", "firebrick", "blue", "coral1"
    )) +
    expand_limits(x = 4.75) +
    labs(x = "Moving Average", y = "% of Stocks Above", color = "") + 
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

graph_ma_uptrend_sector <- function(dat){ # (1) >50d (2) >200d (3) 50d > 200d
  grp <- dat %>% 
    dplyr::distinct(sector) %>% 
    dplyr::arrange(sector) %>% 
    dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "defensive", "growth", "defensive"))
  
  plot_df_a <- dat %>% 
    dplyr::left_join(grp, "sector") %>% 
    dplyr::group_by(group, sector) %>% 
    dplyr::summarise(p = mean(return_50d > 0 & return_200d > 0 & return_50d < return_200d, na.rm = TRUE)*100) %>% 
    dplyr::ungroup() 
  
  plot_df <- dat %>% 
    dplyr::group_by(size) %>% 
    dplyr::summarise(p = mean(return_50d > 0 & return_200d > 0 & return_50d < return_200d, na.rm = TRUE)*100) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      group = "all", 
      sector = plyr::mapvalues(size, c("SML", "MID", "LRG"), c("SP600", "SP400", "SP500")), 
      size = NULL
    ) %>% 
    dplyr::bind_rows(plot_df_a) %>% 
    dplyr::mutate(
      sector = plyr::mapvalues(
        sector, 
        c("Consumer Staples", "Communication Services", "Consumer Discretionary"), 
        c("Staples", "Communications", "Discretionary")
      ),
      sector = forcats::fct_reorder(sector, p)
    )  
  
  plot_df %>% 
    ggplot(aes(sector, p, fill = group)) +
    geom_bar(stat = "identity", color = "black") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_fill_manual(values = c("grey70", "limegreen", "tomato", "dodgerblue")) +
    labs(x = "", y = "% in Uptrend", caption = "(1) > 50DMA, (2) >200DMA, (3) 50DMA > 200DMA") + 
    coord_flip() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

graph_price_avwap_1 <- function(dat, anchor, anchor_label, variable){
  plot_data <- dat %>% 
    dplyr::filter(stringr::str_detect(var, anchor)) %>% 
    dplyr::mutate(var = ifelse(stringr::str_detect(var, "avwap"), "AVWAP", "Price"))
    
  plot_order <- plot_data %>% 
    dplyr::filter(var == variable) %>% 
    dplyr::arrange(p) %>% 
    dplyr::pull(sector)
  
  plot_data %>% 
    dplyr::mutate(
      sector = factor(sector, levels = plot_order), 
      label = anchor_label
    ) %>% 
    ggplot(aes(sector, p, color = group)) +
    geom_point(aes(shape = var), size = 3, stroke = 1) + 
    geom_line(size = 1, alpha = 0.5) + 
    facet_grid(~label) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_shape_manual(values = c(2, 1)) + 
    scale_color_manual(values = c("grey50", "limegreen", "tomato", "dodgerblue")) +
    guides(color = FALSE) +
    labs(x = "", y = "% Above", shape = "") + 
    coord_flip() +
    theme(
      legend.position = "bottom", 
      legend.spacing.x = unit(0.75, 'cm'), 
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

graph_price_avwap <- function(dat, var = "Price"){
  
  grp <- dat %>% 
    dplyr::distinct(sector) %>% 
    dplyr::arrange(sector) %>% 
    dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "defensive", "growth", "defensive"))
  
  plot_df_a <- dat %>% 
    dplyr::left_join(grp, "sector") %>% 
    dplyr::group_by(group, sector) %>% 
    dplyr::summarise(
      p_anchor_1 = mean(return_anchor_1 > 0, na.rm = TRUE) * 100,
      p_anchor_2 = mean(return_anchor_2 > 0, na.rm = TRUE) * 100,
      p_avwap_ytd = mean(return_avwap_ytd > 0, na.rm = TRUE) * 100,
      p_avwap_anchor_1 = mean(return_avwap_anchor_1 > 0, na.rm = TRUE) * 100,
      p_avwap_anchor_2 = mean(return_avwap_anchor_2 > 0, na.rm = TRUE) * 100
    ) %>% 
    dplyr::ungroup() 
  
  plot_df <- dat %>% 
    dplyr::group_by(size) %>% 
    dplyr::summarise(
      p_anchor_1 = mean(return_anchor_1 > 0, na.rm = TRUE) * 100,
      p_anchor_2 = mean(return_anchor_2 > 0, na.rm = TRUE) * 100,
      p_avwap_ytd = mean(return_avwap_ytd > 0, na.rm = TRUE) * 100,
      p_avwap_anchor_1 = mean(return_avwap_anchor_1 > 0, na.rm = TRUE) * 100,
      p_avwap_anchor_2 = mean(return_avwap_anchor_2 > 0, na.rm = TRUE) * 100
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      group = "all", 
      sector = plyr::mapvalues(size, c("SML", "MID", "LRG"), c("SP600", "SP400", "SP500")), 
      size = NULL
    ) %>% 
    dplyr::bind_rows(plot_df_a) %>% 
    tidyr::pivot_longer(dplyr::starts_with("p"), names_to = "var", values_to = "p") 

  # graph and combine
  g1 <- graph_price_avwap_1(plot_df, "anchor_1", anchor_1_msg, var)
  g2 <- graph_price_avwap_1(plot_df, "anchor_2", anchor_2_msg, var)
  
  
  cowplot::plot_grid(
    cowplot::plot_grid(
      g1 + theme(legend.position = "none"),
      g2 + theme(legend.position = "none"),
      nrow = 2, align = 'vl'
    ), 
    cowplot::get_legend(g2),
    nrow = 2, rel_heights = c(2, .2)
  )
}

graph_ma_downtrend_sector <- function(dat){ # (1) <50d (2) <200d (3) 50d < 200d
  grp <- dat %>% 
    dplyr::distinct(sector) %>% 
    dplyr::arrange(sector) %>% 
    dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "defensive", "growth", "defensive"))
  
  plot_df_a <- dat %>% 
    dplyr::left_join(grp, "sector") %>% 
    dplyr::group_by(group, sector) %>% 
    dplyr::summarise(p = mean(return_50d < 0 & return_200d < 0 & return_50d > return_200d, na.rm = TRUE)*100) %>% 
    dplyr::ungroup() 
  
  plot_df <- dat %>% 
    dplyr::group_by(size) %>% 
    dplyr::summarise(p = mean(return_50d < 0 & return_200d < 0 & return_50d > return_200d, na.rm = TRUE)*100) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      group = "all", 
      sector = plyr::mapvalues(size, c("SML", "MID", "LRG"), c("SP600", "SP400", "SP500")), 
      size = NULL
    ) %>% 
    dplyr::bind_rows(plot_df_a) %>% 
    dplyr::mutate(
      sector = plyr::mapvalues(
        sector, 
        c("Consumer Staples", "Communication Services", "Consumer Discretionary"), 
        c("Staples", "Communications", "Discretionary")
      ),
      sector = forcats::fct_reorder(sector, p)
    )  
  
  plot_df %>% 
    ggplot(aes(sector, p, fill = group)) +
    geom_bar(stat = "identity", color = "black") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
    scale_fill_manual(values = c("grey70", "limegreen", "tomato", "dodgerblue")) +
    labs(x = "", y = "% in Downtrend", caption = "(1) < 50DMA, (2) <200DMA, (3) 50DMA < 200DMA") + 
    coord_flip() +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

# misc
graph_ytd_distribution <- function(stocks, etfs){
  sp_percentiles <- stocks %>%
    dplyr::filter(size == "LRG" & !is.na(return_ytd)) %>% 
    dplyr::arrange(return_ytd) %>% 
    dplyr::mutate(i = (1:dplyr::n()) / dplyr::n()) %>% 
    dplyr::select(i, return_ytd)
  
  # label for spy
  sp_avg <- etfs %>% 
    dplyr::filter(ticker == "SPY") %>% 
    dplyr::pull(return_ytd)
  sp500 <- sp_percentiles %>% 
    dplyr::mutate(diff = abs(return_ytd - sp_avg)) %>% 
    dplyr::arrange(diff) %>% 
    head(1) %>% 
    dplyr::mutate(lab = paste0("SPY\n", sprintf("%.1f", sp_avg*100), "%"))
  
  # label for median
  med <- sp_percentiles %>% 
    dplyr::arrange(abs(i - 0.5)) %>% 
    head(1) %>% 
    dplyr::mutate(lab = paste0("Median\n", sprintf("%.1f", return_ytd*100), "%"))
  
  # label locations
  y_loci <- sp_percentiles %>% 
    dplyr::filter(dplyr::between(i, 0.97, 0.99)) %>% 
    head(1) %>% dplyr::pull(return_ytd)
  med_stagger <- if( abs(sp500$i - med$i) > 0.1 ) 1 else 2
  
  sp_percentiles %>% 
    ggplot() +
    geom_area(aes(i*100, return_ytd*100), color = "black", fill = "grey80") + 
    geom_hline(yintercept = 0, color = "grey30") +
    geom_vline(xintercept = 50, linetype = "dashed") +
    geom_vline(data = sp500, aes(xintercept = i*100), linetype = "dashed") +
    geom_label(data = sp500, aes(x = i*100, label = lab), y = y_loci*100, size = 5) +
    geom_label(data = med, aes(x = i*100, label = lab), y = y_loci*100*med_stagger, size = 5) +
    labs(x = "S&P Component Percentile (%)", y = "YTD Return (%)") + 
    theme(
      panel.grid.minor = element_blank()
    )
}

graph_obos <- function(dat, past_years = 2){
  yr <- year(today) - past_years
  plot_data <- stringr::str_glue("SELECT date, ob, os FROM obos WHERE date >= '{yr}-01-01'")%>% 
    query_db() %>% 
    tidyr::pivot_longer(-date)
  
  plot_data %>% 
    ggplot(aes(date, value*100, fill = name, color = name)) +
    geom_hline(yintercept = 0) +
    geom_area() +
    facet_grid(name ~ .) +
    scale_fill_manual(values = c("limegreen", "tomato")) +
    scale_color_manual(values = c("darkgreen", "darkred")) +
    labs(x = "", y = "% Overbought or Oversold") + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

graph_gex <- function(){
  use_n <- 21
  
  gex <- read.csv('https://squeezemetrics.com/monitor/static/DIX.csv') %>% 
    dplyr::mutate(date = lubridate::as_date(date)) %>% 
    tail(use_n)
  
  gex %>% 
    ggplot() +
    geom_tile(aes('a', use_n - as.numeric(date), fill = gex < 0), color = "black") +
    geom_text(aes('a', use_n - as.numeric(date), label = format(date, "%b %d"))) +
    scale_fill_manual(values = c("white", "lawngreen")) +
    labs(x = "", y = "", subtitle = "Negative GEX Signals") +
    theme_gray() + 
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(), 
      aspect.ratio = 3
    ) 
}

graph_anchor_scatter <- function(dat, color){
  
  dat %>% 
    ggplot(aes(return_anchor_1*100, return_anchor_2*100, label = ticker, color = {{color}})) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_vline(xintercept = 0, color = "grey40") +
    geom_abline(intercept = 0, slope = 1, color = "grey40") +
    geom_text() +
    scale_x_continuous(breaks = seq(-100, 100, 10)) + 
    scale_y_continuous(breaks = seq(-100, 100, 10)) + 
    labs(
      x = paste0(anchor_1_msg, " (%)"),
      y = paste0(anchor_2_msg, " (%)")
    ) + 
    theme(
      panel.grid.major = element_blank()
    )
}

