library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(skin="purple",
                    
                    
                    dashboardHeader(title = "Basic dashboard"),
                    ## Sidebar content
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Browse Data", tabName = "Browsedata", icon = icon("dashboard")),
                        menuItem("Plot 1", tabName = "plot1", icon = icon("th")),
                        menuItem("menu 1", tabName= "menu1", icon = icon("refresh"),badgeColor = "green")
                      )
                    ),
                    
                    dashboardBody(
                      tabItems(
                        
                        tabItem(
                          
                          tabName = "Browsedata",
                          selectInput('year', 'Select Year', ""),
                          
                          selectInput('state', 'Select State/UT', ""),
                          
                          selectInput('district', 'District', ""),
                          selectInput('col1', 'y Variable', ""),
                          fileInput('file1', 'Choose CSV File',
                                    accept=c('text/csv', 
                                             'text/comma-separated-values,text/plain', 
                                             '.csv')),
                          
                          fluidRow( box(plotOutput('trail1'),width = 12))
                          
                          
                          
                        ),
                        tabItem(tabName = "plot1",
                                
                                headerPanel('My First Plot'),
                                selectInput('year2', 'Select Year', ""),
                                fluidRow(plotOutput('totalplot',height="600px"))
                                
                                #    # "Empty inputs" - they will be updated after the data is uploaded
                                #    fluidRow(box(
                                #    selectInput('xcol', 'X Variable', ""),
                                #    selectInput('ycol', 'Y Variable', "", selected = "")
                                #    )),
                                #  
                                #  tags$br(),
                                # fluidRow(box( plotOutput('MyPlot'))),
                                #  fluidRow(box(plotOutput('HistPlot')))
                        )
                        
                      ),
                      mainPanel(
                        
                        #tableOutput('contents')
                        
                        
                      )
                      
                    )
                    
                    
)

