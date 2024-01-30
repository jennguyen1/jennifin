# UI Code
# Date: Feb 2023
# Author: Jenny Nguyen
# Email: jnnguyen2@wisc.edu

# open libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)


# header ---------------------------------------------

header <- dashboardHeader(
  title = "jennifin",
  dropdownMenu(
    type = "notifications", 
    icon = paste("Updated as of Market Close", get_file_update_dt()),
    headerText = "",
    badgeStatus = NULL
  )
)

# sidebar ---------------------------------------------
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(
    menuItem("Market Summary", tabName = "main", icon = icon("house")),
    menuItem("Screen ETFs", tabName = "etfs", icon = icon("layer-group")),
    menuItem("Screen Stocks", tabName = "stocks", icon = icon("chart-line")),
    menuItem("Misc Data", tabName = "misc", icon = icon("otter")),
    menuItem("Github Source Code", href = "https://github.com/jennguyen1/jennifin", icon = icon("github"))
  ),
  div(
    p("Copyright (c) 2024 Jennifer N Nguyen under the MIT License"),
    style = "font-size:12px; color:grey; padding: 0px 10px; bottom: 0px; position: absolute"
  )
)


# content ---------------------------------------------
main <- tabItem(
  tabName = "main",
  navbarPage(
    "",
    
    tabPanel("Summary", box(
      width = NULL,
      solidHeader = TRUE, collapsible = FALSE,
      div(DT::DTOutput("tab_performance_major"), style = "margin: 0 auto; max-width: 950px"),
      br()
    )), 
    tabPanel("MA Breadth", box(
      width = NULL,
      div(plotOutput("graph_ma", height = "590px", width = "950px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 950px") 
    )),
    tabPanel("Swing High/Low Breadth", box(
      width = NULL,
      div(plotOutput("graph_price_avwap", height = "590px", width = "950px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 950px") 
    )) 
))

# ETF view
tab_etfs <- tabItem(
  tabName = "etfs",
  navbarPage(
    "Screen ETFs",
    
    tabPanel("Select Tickers", box(
      width = NULL,
      div(DT::DTOutput("tab_select_etf"), style = "margin: 0 auto; max-width: 800px"),
      br(),
      div(actionButton("clear_tab_select_etf", "Deselect All"), style = "text-align: right")
    )),
    tabPanel("Leaders & Laggards", box(
      width = NULL, 
      div(plotly::plotlyOutput("graph_lead_lag_etf", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px") 
    )),
    tabPanel("Returns from Key Dates", box(
      width = NULL, 
      div(plotly::plotlyOutput("graph_scatter_etf", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px") 
    )),
    tabPanel("Performance", box(
      width = NULL, 
      div(DT::DTOutput("tab_performance_etf"), style = "margin: 0 auto; max-width: 1000px")
    ))
  )
)

# stock view
tab_stocks <- tabItem(
  tabName = "stocks",
  navbarPage(
  "Screen Stocks",
    
    tabPanel("Filter", box(
      width = NULL, fluidRow(
        column(
          width = 9, 
          plotly::plotlyOutput("graph_lead_lag_stock", height = "500px", width = "800px")
        ),
        uiOutput("stocks_display") %>% shinycssloaders::withSpinner(type = 7)
      )
    )),
    tabPanel("Screen History", box(
      width = NULL,
      div(plotly::plotlyOutput("graph_stock_screen_hist", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px")
    )),
    tabPanel("Top Screened Industries", box(
      width = NULL,
      div(plotOutput("graph_stock_screen_industry", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px")
    ))
))

# miscellaneous
tab_misc <- tabItem(
  tabName = "misc",
  navbarPage(
    "Misc Data",
    
    tabPanel("SP500 YTD Distribution", box(
      width = NULL,
      div(plotOutput("graph_ytd_distribution", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px") 
    )),
    tabPanel("Overbought/Oversold", box(
      width = NULL,
      div(plotOutput("graph_obos", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px") 
    )),
    tabPanel("Breadth Uptrend", box(
      width = NULL,
      div(plotOutput("graph_breadth_uptrend_history", height = "500px", width = "950px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 950px") 
    )),
    tabPanel("GEX", box(
      width = NULL, 
      plotOutput("graph_gex", height = "500px") %>% shinycssloaders::withSpinner(type = 7)
    )),
    
    tabPanel("52w High/Low by Sector", box(
      width = NULL,
      div(plotOutput("graph_hilo_sector", height = "500px", width = "800px") %>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 800px")
    ))
))


# assemble ---------------------------------------------

# UI functions setup
function(request){
  dashboardPage(
    skin = "purple",
    header,
    sidebar, 
    dashboardBody(
      includeCSS("www/custom.css"),
      
      tabItems(
        main,
        tab_etfs,
        tab_stocks,
        tab_misc
      ),
      div(
        p("Content contained or made available through the app is provided for informational purposes only and does not constitute financial advice. No one should make any financial decisions without first conducting his or her own research and due diligence. To the maximum extent permitted by law, JN disclaims any and all liability in the event any information and/or analysis prove to be inaccurate, incomplete, unreliable, or result in any investment or other losses. You should consult with a professional to determine what may be the best for your individual needs."), 
        style = "font-size:12px; color:grey")
    )
  )
}

