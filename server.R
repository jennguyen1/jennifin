# Server Code
# Date: Feb 2023
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shinyWidgets)
library(plotly)
library(DT)


# server functions
shinyServer(function(input, output) {
  
  ## MAJORS SUMMARY ==================================================================
  
  output$tab_performance_major <- DT::renderDT({
    display_table_summary(etfs, stocks)
  })
  
  
  ## ETFS ==================================================================
  
  # ETF selection
  output$tab_select_etf <- DT::renderDT({
    disp_data <- etfs %>% 
      dplyr::select(ticker, desc, type, category, category2) %>% 
      dplyr::mutate_at(dplyr::vars(type, category, category2), factor)
    
    disp_data %>% 
      DT::datatable(
        rownames = FALSE,
        colnames = c(
          "Ticker" = "ticker", 
          "Description" = "desc",
          "Type" = "type",
          "Category" = "category",
          "Category 2" = "category2"
        ), 
        class = 'cell-border compact hover',
        filter = list(position = "top", clear = FALSE),
        options = list(
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(disp_data)-1))),
          pageLength = nrow(disp_data),
          scrollY = 377
        )
      ) %>% 
      DT::formatStyle(1, fontWeight = "bold") 
  })
  
  proxy_dt_etf = dataTableProxy("tab_select_etf")
  observeEvent(input$clear_tab_select_etf, {
    proxy_dt_etf %>% selectRows(NULL)
  })
  
  # ETF graph & tables
  output$graph_lead_lag_etf <- plotly::renderPlotly({
    sub_tickers <- etfs$ticker[input$tab_select_etf_rows_selected]
    ggplotly( graph_lead_lag(etfs, sub = sub_tickers, color = category) )
  })
  
  output$tab_performance_etf <- DT::renderDT({
    sub_tickers <- etfs$ticker[input$tab_select_etf_rows_selected]
    tabulate_performance_etfs(etfs, sub = sub_tickers)
  })


  ## STOCKS ================================================================
  
  # ta screen description
  observeEvent(input$show_ta_msg, {
    showModal(modalDialog(
      title = "TA Screening Process", 
      tags$ol(
        tags$li("Above 200D MA"),
        tags$li("At or above", anchor_msg, "price"),
        tags$li("Outperformance relative to SPY (+/- 1%) over trailing month"),
        tags$li("Outperformance relative to corresponding sector (+/- 1%) over trailing month"),
        tags$li("Increasing 200D SMA slope over past 2 weeks"),
        tags$li("Bullish momentum regime with no oversold RSI14 reading in trailing 3 months"),
        tags$li("Sort by proximity to 52-week highs")
      ),
      p("Follow up by examining the charts:"),
      tags$ul(
        tags$li("Price chart"),
        tags$li("Price relative to SPY and relevant sector"),
        tags$li("RSI regime on a daily and weekly time frame"),
        tags$li("Support/resistance levels using AVWAP and Fibonacci")
      ),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  # stock selection
  output$select_stock_size <- renderUI({
    checkboxGroupButtons(
      inputId = "screen_stock_size",
      label = "Market Cap Size",
      choices = unique(stocks$size),
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  })
  
  output$select_stock_sector <- renderUI({
    checkboxGroupButtons(
      inputId = "screen_stock_sector",
      label = "Sector",
      choices = unique(stocks$sector),
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
  })
  
  output$select_stock_in_ta_screen <- renderUI({
    awesomeCheckbox(
      inputId = "screen_stock_in_ta_screen",
      label = "Apply Screen", 
      value = TRUE
    )
  })
  
  output$select_stock_msg <- renderUI({
    sub_tickers <- stocks %>%
      filter_stocks(
        sz = input$screen_stock_size, sct = input$screen_stock_sector, 
        ta_scn = input$screen_stock_in_ta_screen, ta_lst = stocks_ta_screen$ticker
      )
    paste("Showing", length(sub_tickers), "stocks")
  })
  
  # stock graph & tables
  output$graph_lead_lag_stock <- plotly::renderPlotly({
    sub_tickers <- stocks %>%
      filter_stocks(
        sz = input$screen_stock_size, sct = input$screen_stock_sector, 
        ta_scn = input$screen_stock_in_ta_screen, ta_lst = stocks_ta_screen$ticker
      )
    ggplotly( graph_lead_lag(stocks, sub = sub_tickers, color = sector) )
  })
  
  output$tab_performance_stock <- DT::renderDT({
    sub_tickers <- stocks %>%
      filter_stocks(
        sz = input$screen_stock_size, sct = input$screen_stock_sector, 
        ta_scn = input$screen_stock_in_ta_screen, ta_lst = stocks_ta_screen$ticker
      )
    if(input$screen_stock_in_ta_screen){
      tabulate_performance_stocks(stocks_ta_screen, sub_tickers)
    } else{
      tabulate_performance_stocks(stocks, sub_tickers)
    }
  })
  
  output$graph_stock_screen_hist <- plotly::renderPlotly({
    plot_data <- readr::read_csv("data/stats_ta_screen.csv") %>% 
      tidyr::pivot_longer(-date, names_to = "sector", values_to = "percent")
    
    plot_order <- plot_data %>% 
      dplyr::filter(date == max(date)) %>% 
      dplyr::mutate(serotype = forcats::fct_reorder(sector, dplyr::desc(percent))) %>% 
      dplyr::pull(serotype) %>% levels()
    
    g <- plot_data %>% 
      dplyr::mutate(sector = factor(sector, levels = plot_order)) %>% 
      ggplot(aes(date, percent)) +
      # geom_area(fill = "grey70") +
      geom_bar(stat = "identity", color = "black", fill = "grey70") +
      facet_wrap(~sector) +
      scale_x_date(date_breaks = "1 month") + 
      labs(x = "", y = "% of Sector in TA Screen") + 
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 15, hjust = 1, vjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
      )
    
    plotly::ggplotly(g)
  })
  
  output$graph_stock_screen_industry <- renderPlot({
    plot_data <- dplyr::full_join(
      stocks_ta_screen %>% dplyr::count(industry), 
      stocks %>% dplyr::count(sector, industry),
      "industry",
      suffix = c("_num", "_den")
    ) %>% 
      dplyr::mutate(
        perc = n_num / n_den * 100,
        perc = ifelse(is.na(perc), 0, perc),
        industry = forcats::fct_reorder(factor(industry), perc)
      ) %>% 
      dplyr::arrange(industry) %>% 
      tail(25)
    
    plot_data %>% 
      ggplot(aes(industry, perc, fill = sector)) +
      geom_bar(stat = "identity", color = "black") +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "Industry", y = "% of Industry in TA Screen", fill = "Sector") +
      coord_flip() + 
      theme(
        legend.position = "top", 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

  # ui output
  output$stocks_display <- renderUI({
    column(
      width = 3, 
      uiOutput("select_stock_size"),
      uiOutput("select_stock_sector"),
      hr(),
      strong("Technical Analysis Screen"), 
      actionLink("show_ta_msg", "", icon = icon("circle-info")),
      uiOutput("select_stock_in_ta_screen"),
      hr(),
      uiOutput("select_stock_msg")
    )
  })

  
  ## MISC ================================================================
  
  output$graph_ytd_distribution <- renderPlot({
    
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
      dplyr::mutate(lab = paste0("SPY\n", sp_avg*100, "%"))
    
    # label for median
    med <- sp_percentiles %>% 
      dplyr::arrange(abs(i - 0.5)) %>% 
      head(1) %>% 
      dplyr::mutate(lab = paste0("Median\n", return_ytd*100, "%"))
    
    # label locations
    y_loci <- sp_percentiles %>% 
      dplyr::filter(dplyr::between(i, 0.97, 0.99)) %>% 
      head(1) %>% dplyr::pull(return_ytd)
    
    sp_percentiles %>% 
      ggplot() +
      geom_area(aes(i, return_ytd*100), color = "black", fill = "grey80") + 
      geom_hline(yintercept = 0, color = "grey30") +
      geom_vline(xintercept = 0.5, linetype = "dashed") +
      geom_vline(data = sp500, aes(xintercept = i), linetype = "dashed") +
      geom_label(data = sp500, aes(x = i, label = lab), y = y_loci*100, size = 5) +
      geom_label(data = med, aes(x = i, label = lab), y = y_loci*100, size = 5) +
      labs(x = "S&P Component", y = "YTD Return (%)") + 
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  output$graph_trend_strength <- renderPlot({
    # todo
  })
  
  output$graph_ma_by_group <- renderPlot({
    
    grp <- stocks %>% 
      dplyr::distinct(sector) %>% 
      dplyr::arrange(sector) %>% 
      dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "growth", "growth", "defensive"))
    
    df <- stocks %>% 
      dplyr::left_join(grp, "sector") %>% 
      dplyr::select(ticker, sector, group, dplyr::ends_with("d"), -dplyr::matches("ytd")) %>% 
      dplyr::mutate(dplyr::across(dplyr::ends_with("d"), \(x) x > 0))
    
    sdf <- df %>% # all
      dplyr::summarise(across(
        dplyr::ends_with("d"),
        \(x) mean(x, na.rm = TRUE)
      )) %>% 
      dplyr::mutate(group = "all")
    
    adf <- df %>% # by sector
      dplyr::group_by(group) %>% 
      dplyr::summarise(dplyr::across(
        dplyr::ends_with("d"),
        \(x) mean(x, na.rm = TRUE)
      ))
    
    dplyr::bind_rows(sdf, adf) %>% 
      tidyr::pivot_longer(-group) %>% 
      dplyr::mutate(days = factor(readr::parse_number(name))) %>% 
      ggplot(aes(days, value*100, group = group, color = group)) +
      geom_line(linewidth = 1.5) +
      geom_hline(yintercept = c(0, 100), color = "grey50") +
      geom_hline(yintercept = 50, color = "grey50", linetype = "dashed") +
      scale_color_manual(values = c("black", "limegreen", "tomato", "dodgerblue")) +
      labs(x = "Moving Average", y = "% of Stocks Above", color = "") + 
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()
      )
  })
  
  output$graph_ma_by_sector <- renderPlot({
    
    grp <- stocks %>% 
      dplyr::distinct(sector) %>% 
      dplyr::arrange(sector) %>% 
      dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "growth", "growth", "defensive"))
    
    df <- stocks %>% 
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
        )
      ) 
    
    plot_df %>% 
      ggplot() +
      geom_line(aes(days, value*100, group = sector, color = group), linewidth = 1.25) +
      geom_text(data = plot_df %>% dplyr::filter(days == 200), aes(3.1, value*100, label = sector), hjust = 0) + 
      geom_hline(yintercept = c(0, 100), color = "grey50") +
      geom_hline(yintercept = 50, color = "grey50", linetype = "dashed") +
      facet_grid(~group) + 
      scale_color_manual(values = c("black", "limegreen", "tomato", "dodgerblue")) +
      expand_limits(x = 4.75) +
      labs(x = "Moving Average", y = "% of Stocks Above", color = "") + 
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank()
      )
    
  })
  
  output$graph_hilo_sector <- renderPlot({
    
    grp <- stocks %>% 
      dplyr::distinct(sector) %>% 
      dplyr::arrange(sector) %>% 
      dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "growth", "growth", "defensive"))
    
    plot_df_a <- stocks %>% 
      dplyr::left_join(grp, "sector") %>% 
      dplyr::group_by(group, sector) %>% 
      dplyr::summarise(p = mean(abs(below_52w_high) < abs(above_52w_low), na.rm = TRUE)*100) %>% 
      dplyr::ungroup() 
    
    plot_df <- stocks %>% 
      dplyr::group_by(size) %>% 
      dplyr::summarise(p = mean(abs(below_52w_high) < abs(above_52w_low), na.rm = TRUE)*100) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        group = "all", 
        sector = plyr::mapvalues(size, c("SML", "MID", "LRG"), c("SP600", "SP400", "SP500")), 
        size = NULL
      ) %>% 
      dplyr::bind_rows(plot_df_a) %>% 
      dplyr::mutate(sector = forcats::fct_reorder(sector, p))  
      
    plot_df %>% 
      ggplot(aes(sector, p, fill = group)) +
      geom_bar(stat = "identity", color = "black") +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(values = c("grey70", "limegreen", "tomato", "dodgerblue")) +
      labs(x = "", y = "% Closer to 52W Highs Than Lows") + 
      coord_flip() +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank()
      )
  })
  
  output$graph_gex <- renderPlot({
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
  })
  
})
