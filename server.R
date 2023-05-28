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
        tags$li("At or above", anchor_hi_msg, "price"),
        tags$li("Outperformance relative to SPY over trailing month"),
        tags$li("Increasing 200D SMA slope over past 2 weeks"),
        tags$li("Bullish momentum regime with no oversold RSI14 reading in trailing 3 months"),
        tags$li("Sort by proximity by to 52-week highs")
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
      value = FALSE
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
  
  output$graph_breadth <- renderPlot({
    
    grp <- stocks %>% 
      dplyr::distinct(sector) %>% 
      dplyr::arrange(sector) %>% 
      dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "growth", "cyclical", "growth", "defensive"))
    
    df <- stocks %>% 
      dplyr::left_join(grp, "sector") %>% 
      dplyr::select(ticker, sector, group, dplyr::ends_with("d"), -dplyr::matches("ytd")) %>% 
      dplyr::mutate(dplyr::across(dplyr::ends_with("d"), \(x) x > 0))
    
    sdf <- df %>% 
      dplyr::summarise(across(
        dplyr::ends_with("d"),
        \(x) mean(x, na.rm = TRUE)
      )) %>% 
      dplyr::mutate(group = "all")
    
    adf <- df %>% 
      dplyr::group_by(group) %>% 
      dplyr::summarise(dplyr::across(
        dplyr::ends_with("d"),
        \(x) mean(x, na.rm = TRUE)
      ))
    
    dplyr::bind_rows(sdf, adf) %>% 
      tidyr::pivot_longer(-group) %>% 
      dplyr::mutate(days = factor(readr::parse_number(name))) %>% 
      ggplot(aes(days, value, group = group, color = group)) +
      geom_line(linewidth = 1.5) +
      geom_hline(yintercept = c(0, 1), color = "grey50") +
      geom_hline(yintercept = 0.5, color = "grey50", linetype = "dashed") +
      scale_color_manual(values = c("black", "limegreen", "tomato", "dodgerblue")) +
      labs(x = "Moving Average", y = "Proportion of Stocks Above", color = "") + 
      theme_bw() +
      theme(
        legend.position = "top",
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(),
        text = element_text(family = "Arial", size = 18)
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
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(), 
        aspect.ratio = 3
      ) 
  })
  
  
})
