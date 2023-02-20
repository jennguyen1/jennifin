# Server Code for Calculator
# Date: Feb 2023
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shinyWidgets)
library(plotly)
library(DT)

# pull in code & data
source("www/util.R")


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
        filter = list(
          position = "top",
          clear = FALSE
        ),
        options = list(
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = 0:4)),
          pageLength = nrow(disp_data),
          scrollY = 400
        )
      ) %>% 
      formatStyle(1, fontWeight = "bold") 
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
  
  # stock graph & tables
  output$graph_lead_lag_stock <- plotly::renderPlotly({
    sub_tickers <- stocks %>% 
      filter_stocks(sz = input$screen_stock_size, sct = input$screen_stock_sector) 
    ggplotly( graph_lead_lag(stocks, sub = sub_tickers, color = sector) )
  })
  
  output$tab_performance_stock <- DT::renderDT({
    sub_tickers <- stocks %>% 
      filter_stocks(sz = input$screen_stock_size, sct = input$screen_stock_sector) 
    tabulate_performance_stocks(stocks, sub_tickers)
  })

})
