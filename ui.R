library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(DT)
library(lubridate)
library(plotly)
library(readxl)


# UI 
ui <- dashboardPage(
  dashboardHeader(title = "LAPD Crime Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Crime Map", tabName = "map", icon = icon("map-marker")),
      menuItem("Time Analysis", tabName = "time", icon = icon("clock")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Data Table", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Page
      tabItem(tabName = "home",
              fluidRow(
                column(width = 12, 
                       box(width = NULL, status = "primary",
                           div(style = "text-align: center; padding: 20px;",
                               img(src = "lapd.jpeg", 
                                   height = 200),
                               h2("Los Angeles Police Department", style = "font-weight: bold;"),
                               h4("Crime Analysis Dashboard")
                           )
                       )
                )
              ),
              
              fluidRow(
                column(width = 6,
                       box(title = "About Los Angeles", status = "primary", solidHeader = TRUE, width = NULL,
                           p("Los Angeles is the largest city in California and the second most populous city in the United States after New York City.
               With a 2020 population of approximately 3.9 million, Los Angeles is known for its Mediterranean climate, ethnic and cultural 
               diversity, Hollywood entertainment industry, and sprawling metropolitan area."),
                           p("The city spans over 469 square miles (1,210 km²) and is the seat of Los Angeles County, 
               the most populous county in the United States. Los Angeles is known for its diverse economy, 
               including international trade, entertainment, technology, petroleum, fashion, apparel, and tourism.")
                       )
                ),
                
                column(width = 6,
                       box(title = "About LAPD", status = "primary", solidHeader = TRUE, width = NULL,
                           p("The Los Angeles Police Department (LAPD) is the police department of Los Angeles, California. 
               With 9,974 officers and 3,000 civilian staff, it is the third-largest municipal police department in the United States, 
               after the New York City Police Department and the Chicago Police Department."),
                           p("The LAPD is divided into 21 geographic areas, each managed by a Community Police Station. 
               These areas are organized into four command bureaus: Central Bureau, South Bureau, Valley Bureau, and West Bureau.
               This dashboard provides analysis of crime data across these areas to help understand patterns and trends.")
                       )
                )
              ), 
              
              fluidRow(
                box(title = "Using This Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                    p("This interactive dashboard allows you to explore crime data from the Los Angeles Police Department. 
        Use the sidebar menu to navigate between different views:"),
                    tags$ul(
                      tags$li(tags$b("Overview:"), "Get summary statistics on crime rates, types, and distribution."),
                      tags$li(tags$b("Crime Map:"), "Explore the geographic distribution of crimes across Los Angeles."),
                      tags$li(tags$b("Time Analysis:"), "Analyze crime patterns by time of day, day of week, and month."),
                      tags$li(tags$b("Demographics:"), "Examine victim demographics and weapon usage."),
                      tags$li(tags$b("Data Table:"), "View and search the raw data.")
                    ),
                    p("Use the filters in the sidebar to focus on specific areas, crime types, or date ranges."),
                    actionButton("get_started", "Get Started", 
                                 icon = icon("arrow-right"), 
                                 style = "color: #fff; background-color: #3c8dbc; border-color: #3c8dbc")
                )
              )
      ),
      
      # Overview
      tabItem(tabName = "overview",
              h2("Crime Overview Statistics"),
              
              # Add filters here
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE,
                    fluidRow(
                      column(width = 2,
                         selectInput("year_start", "From Year:",
                                     choices = NULL,
                                     selected = NULL)
                         ),
                      column(width = 2,
                         selectInput("year_end","To Year:",
                                      choices = NULL,
                                      selected = NULL)
                         ),
                      column(width = 3,
                             selectInput("area", "Area:",
                                         choices = NULL,
                                         selected = "All")
                      ),
                      column(width = 3,
                             selectInput("crime_type", "Crime Type:",
                                         choices = NULL,
                                         selected = "All")
                      ),
                      column(width = 2, style = "margin-top: 25px;",
                             actionButton("apply_filters", "Apply Filters", 
                                          icon = icon("filter"),
                                          style = "color: #fff; background-color: #3c8dbc; border-color: #3c8dbc")
                      )
                    )
                )
              ),
              
              # Stats boxes
              fluidRow(
                # Total Crimes
                valueBoxOutput("total_crimes", width = 3),
                # Clearance Rate
                valueBoxOutput("clearance_rate", width = 3),
                # Most Common Crime
                valueBoxOutput("most_common_crime", width = 3),
                # Highest Crime Area
                valueBoxOutput("highest_crime_area", width = 3)
              ),
              
    
              fluidRow(
                # Crime Types
                box(title = "Top Crime Types", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("crime_types_chart", height = "300px")),
              ),
              
              fluidRow(
                # Monthly Trend
                box(title = "Monthly Crime Trend", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("monthly_trend_chart", height = "300px")),
                
                # Case Status Distribution
                box(title = "Case Status Distribution", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("case_status_chart", height = "300px"))
              ),
              
              # Income vs. Crime row
              fluidRow(
                box(title = "Median Household Income vs. Crime Rates", status = "primary", solidHeader = TRUE, width = 12,
                    plotlyOutput("income_crime_chart", height = "300px"))
              )
      ),
      
      # Crime Map Tab
      tabItem(tabName = "map",
              h2("Crime Map Tab"),
              p("This is the crime map tab. Add your content here.")
      ),
      
      # Time Analysis Tab
      tabItem(tabName = "time",
              h2("Time Analysis Tab"),
              p("This is the time analysis tab. Add your content here.")
      ),
      
      # Demographics Tab 
      tabItem(tabName = "demographics",
              h2("Demographics Tab"),
              p("This is the demographics tab. Add your content here.")
      ),
      
      # Data Table Tab 
      tabItem(tabName = "data",
              h2("Data Table Tab"),
              p("Explore the raw crime data."),
              box(width = NULL,
                  DT::dataTableOutput("crime_table"))
      )
    )
  )
)

