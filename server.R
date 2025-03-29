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
  
  output$graph_scatter_etf <- plotly::renderPlotly({
    sub_tickers <- etfs$ticker[input$tab_select_etf_rows_selected]
    if(length(sub_tickers) == 0){
      sub_tickers <- etfs$ticker
    }
    
    plotly::ggplotly(
      etfs %>% 
        dplyr::filter(ticker %in% sub_tickers) %>% 
        graph_anchor_scatter(color = category)
    )
  })


  ## STOCKS ================================================================

  # ta screen description
  observeEvent(input$show_ta_msg, {
    showModal(modalDialog(
      title = "TA Screening Process", 
      tags$ol(
        tags$li("In an uptrend defined by being above 50DMA, 200D MA, and 50D MA > 200D MA"), 
        tags$li("At or above", anchor_msg, "AVWAP"),
        tags$li("Outperformance relative to SPY (+/- 1%) over trailing month"),
        tags$li("Outperformance relative to corresponding sector (+/- 1%) over trailing month"),
        tags$li("Sort by proximity to 52-week highs")
      ),
      p("Follow up by examining the charts:"),
      tags$ul(
        tags$li("Price chart"),
        tags$li("Relative price chart"),
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
  
  output$download_ta_stocks <- downloadHandler(
    filename = function() paste0("stock_list_", stringr::str_replace_all(Sys.Date(), "-", "_"), ".csv"),
    content = function(file){
      sub_tickers <- stocks %>%
        filter_stocks(
          sz = input$screen_stock_size, sct = input$screen_stock_sector, 
          ta_scn = input$screen_stock_in_ta_screen, ta_lst = stocks_ta_screen$ticker
        )
      
      stocks_ta_screen %>% 
        dplyr::filter(ticker %in% sub_tickers) %>% 
        purrr::set_names(
          colnames(.) %>% 
            stringr::str_to_title() %>% 
            plyr::mapvalues(., 
              c("Return_anchor_1", "Return_avwap_anchor_1", "Return_avwap_ytd", "Days_since_os", "Return_above_52w_lo", "Return_below_52w_hi"), 
              c(paste(anchor_msg, "%"), paste(anchor_msg, "AVWAP %"), "YTD AVWAP %","Days Since OS", "% Above 52W Low", "% Below 52W High")
            )
        ) %>% 
        dplyr::mutate_at(dplyr::vars(dplyr::matches("%")), ~ round(.x*100, 1)) %>% 
        readr::write_csv(file) 
    }
  )
  
  output$download_ta_stocks_ui <- renderUI({
    req(input$screen_stock_in_ta_screen)
    if(input$screen_stock_in_ta_screen){
      downloadLink("download_ta_stocks", "Download")
    }
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
    
    tabulate_performance_stocks(stocks, sub_tickers)
  })
  
  output$graph_stock_screen_hist <- plotly::renderPlotly({
    plot_data <- readr::read_csv("data/stats_ta_screen.csv") %>% 
      tidyr::pivot_longer(-date, names_to = "sector", values_to = "percent") %>% 
      dplyr::filter(month(date) >= (month(today())-3))
    
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
      uiOutput("select_stock_msg"),
      uiOutput("download_ta_stocks_ui")
    )
  })

  
  ## GRAPHS ================================================================
  
  output$graph_ytd_distribution <- renderPlot({
    graph_ytd_distribution(stocks, etfs)
  })
  
  output$graph_breadth_uptrend_history <- renderPlot({
    graph_ma_uptrend_by_group(s, past_years = 2)
  })
  
  output$graph_obos <- renderPlot({
    graph_obos(past_years = 2) 
  })
  
  output$graph_gex <- renderPlot({
    graph_gex()
  })
  
  output$graph_ma <- renderPlot({
    cowplot::plot_grid(
      graph_ma_by_sector(stocks),
      graph_ma_uptrend_sector(stocks),
      nrow = 2, rel_heights = c(1.25,1), 
      align = 'v', axis = "l"
    )
  })
  
  output$graph_up_down_trend <- renderPlot({ # note: opposite of the graph_ma 2nd plot, can alternate when environment changes
    graph_ma_downtrend_sector(stocks)
  })
  
  output$graph_price_avwap <- renderPlot({ 
    graph_price_avwap(stocks, input$var_pa_graph)
  })
  
  output$graph_hilo_sector <- renderPlot({ 
    
    grp <- stocks %>% 
      dplyr::distinct(sector) %>% 
      dplyr::arrange(sector) %>% 
      dplyr::mutate(group = c("growth", "growth", "defensive", "cyclical", "cyclical", "defensive", "cyclical", "cyclical", "defensive", "growth", "defensive"))
    
    plot_df_a <- stocks %>% 
      dplyr::left_join(grp, "sector") %>% 
      dplyr::group_by(group, sector) %>% 
      dplyr::summarise(p = mean(abs(return_below_52w_hi) < abs(return_above_52w_lo), na.rm = TRUE)*100) %>% 
      dplyr::ungroup() 
    
    plot_df <- stocks %>% 
      dplyr::group_by(size) %>% 
      dplyr::summarise(p = mean(abs(return_below_52w_hi) < abs(return_above_52w_lo), na.rm = TRUE)*100) %>% 
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
  
})
