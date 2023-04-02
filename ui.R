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
    icon = paste("Updated ", get_file_update_dt()),
    headerText = "",
    badgeStatus = NULL
  )
)

# sidebar ---------------------------------------------
sidebar <- dashboardSidebar(
  collapsed = FALSE,
  sidebarMenu(
    menuItem("Major Markets", tabName = "main", icon = icon("house")),
    menuItem("Screen ETFs", tabName = "etfs", icon = icon("layer-group")),
    menuItem("Screen Stocks", tabName = "stocks", icon = icon("chart-line")),
    menuItem("Github Source Code", href = "https://github.com/jennguyen1/jennifin", icon = icon("github"))
  ),
  div(
    p("Copyright (c) 2023 Jennifer N Nguyen under the MIT License"),
    style = "font-size:12px; color:grey; padding: 0px 10px; bottom: 0px; position: absolute"
  )
)


# content ---------------------------------------------
main <- tabItem(
  tabName = "main",
  verticalLayout(
    box(
      width = NULL,
      title = "Major Market Summary", 
      solidHeader = TRUE, collapsible = FALSE,
      p(),
      div(DT::DTOutput("tab_performance_major")%>% shinycssloaders::withSpinner(type = 7), style = "margin: 0 auto; max-width: 950px"),
      p(), br(), p()
    )
  ) 
)

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
    tabPanel("Performance", box(
      width = NULL, 
      div(DT::DTOutput("tab_performance_etf"), style = "margin: 0 auto; max-width: 950px")
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
    tabPanel("Performance", box(
      width = NULL,
      div(DT::DTOutput("tab_performance_stock"), style = "margin: 0 auto; max-width: 950px")
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
        tab_stocks
      ),
      div(
        p("Content contained or made available through the app is provided for informational purposes only and does not constitute financial advice. No one should make any financial decisions without first conducting his or her own research and due diligence. To the maximum extent permitted by law, JN disclaims any and all liability in the event any information and/or analysis prove to be inaccurate, incomplete, unreliable, or result in any investment or other losses. You should consult with a professional to determine what may be the best for your individual needs."), 
        style = "font-size:12px; color:grey")
    )
  )
}

