library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)

dashboardPage(skin = "green",
dashboardHeader(title = "Dog Care"),
  dashboardSidebar(
  
    sidebarMenu(
    #menuItem("Home", tabName = "home", icon = icon("fas fa-home")),
    menuItem("Data", tabName = "data", icon = icon("fas fa-database")),
    menuItem("Adoption", tabName = "adoption", icon = icon("paw")),
    menuItem("Diseases", tabName = "diseases", icon = icon("fas fa-ambulance")),
    menuItem("Vaccination", tabName = "vaccination", icon = icon("syringe")),
    menuItem("Goverment Fund", tabName = "Goverment_Fund", icon = icon("fas fa-donate")),
    menuItem("Gender", tabName = "Gender", icon = icon("fas fa-venus-double")),
    menuItem("Status", tabName = "status", icon = icon("fas fa-question-circle")),
    menuItem("Linear", tabName = "linear", icon = icon("fas fa-chart-line"),
             menuSubItem("All",tabName = "all"),
             menuSubItem("Dog",tabName = "doglinear"),
             menuSubItem("Puppy",tabName = "diseaselinear")
             # menuSubItem("Dog",tabName = "dog"),
             # menuSubItem("Puppy",tabName = "puppy"),
             # menuSubItem("diseases",tabName = "dogdisease")
             ),
    menuItem("Pie Chart", tabName = "pie", icon = icon("fas fa-chart-pie"),
             menuSubItem("Disease",tabName = "diseasepie")),
            # menuSubItem("Status",tabName = "statuspie")),
    menuItem("Cluster",tabName = "cluster",icon = icon("fas fa-object-group")),
    #menuItem("map",tabName = "map",icon = icon("fas fa-object-group")),
    menuItem("Agreegate Function",tabName = "agreegate",icon = icon("fas fa-object-group"),
             menuSubItem("Death Average",tabName = "deathagreegate")
             #menuSubItem("Death Average of Status",tabName = "deathstatus")
             )
    # menuItem("linear",tabName = "linear",icon = icon("fas fa-chart-line"),
    #           
    #          )
    
    
    
  )),
  dashboardBody(
    tabItems(tabItem(tabName = "diseaselinear",
                     fluidRow(box(title = "Linear Graph For Diseases (Puppy Vs Year)",height = 50,width = 12,background = "black",solidHeader = TRUE)),
                     width = 500,
                                     fluidRow(box(background = "black",
                                                  solidHeader = TRUE,
                                                  collapsible = TRUE,status = "success",title = "Select Diseases",
                                                  selectInput("diseaselinear","Diseases",""))),
                                     fluidRow(box(background = "black",width=12,plotlyOutput("diseaselinearplot")))
                                    # br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summaryvacc")))
              ),
             tabItem(tabName = "doglinear",
                     fluidRow(box(title = "Linear Graph For Diseases (Dog Vs Year)",height = 50,width = 12,background = "black",solidHeader = TRUE)),
                     width = 500,
                     fluidRow(box(background = "black",
                                  solidHeader = TRUE,
                                  collapsible = TRUE,status = "success",title = "Select Diseases",
                                  selectInput("diseasedoglinear","Diseases",""))),
                     fluidRow(box(background = "black",width=12,plotOutput("diseasedoglinearplot")))
                     #br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summarydog")))
             ),
       # tabItem(tabName = "map",
       #        fluidRow(plotOutput("map"))),
      tabItem(tabName = "deathagreegate",
              fluidRow(box(title = "Death Average",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(box(width = 12,valueBoxOutput("avgyr_",width = 4),valueBoxOutput("avgmonth",width = 4))),
              fluidRow(box(background = "black",width=12,plotlyOutput("agreegate",height = 500)))
              #fluidRow(box(title = "Summarise",background = "black",width=3,plotOutput("summariseagreegate",height = 500)))
              ),
      # tabItem(tabName = "deathstatus",
      #         fluidRow(box(title = "Agreegate",height = 50,width = 12,background = "black",solidHeader = TRUE)),
      #         fluidRow(box(title = "Select Year",background = "black",
      #                      solidHeader = TRUE,
      #                      collapsible = TRUE,status = "success",selectInput('yrdeath','Year',''))),
      #         fluidRow(box(background = "black",width=12,plotlyOutput("statusagreegate",height = 500)))
      #         #fluidRow(box(title = "Summarise",background = "black",width=3,plotOutput("summariseagreegate",height = 500)))
      # ),
      
     # tabItem(tabName = "home"),

      tabItem(tabName = "data",
              fluidRow(box(title = "Choose Csv File",status = "success",background = "black",
                solidHeader = TRUE,
                           collapsible = TRUE,
                fileInput('file1', 'CSV File',
                          accept=c('text/csv', 
                                   'text/comma-separated-values,text/plain', 
                                   '.csv'))
                #selectInput('animalid', 'Select animal', "")
              )),
              fluidRow(box(title="Wordcloud"
                             ,background = "black",width=12,plotOutput("wordcloud")))
              #fluidRow(tableOutput('test'))
              ),
      #vaccination
      tabItem(tabName = "vaccination",
              fluidRow(box(title = "Area Wise Vaccination",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(box(background = "black",
                           solidHeader = TRUE,
                           collapsible = TRUE,status = "success",title = "Select vaccination Year",
              #   dateRangeInput('dateRange',
              #                             label = 'Date range input: yyyy-mm-dd',
              #                             start = Sys.Date() - 2, end = Sys.Date() + 2
              # )),
              selectInput('vaccyr', 'vaccination Year', "")),valueBoxOutput("vaccvalue",width = 3)),
              fluidRow(plotlyOutput("vaccplot",height = 500))),
              
      #adoption
       tabItem(tabName = "adoption",
               fluidRow(box(title = "Adoption Count(Annually)",height = 50,width = 12,background = "black",solidHeader = TRUE)),
               fluidRow(box(background = "black",width=12,plotlyOutput("plot1",height = 500)))
               ),
      #donation
      tabItem(tabName = "Goverment_Fund",
              fluidRow(box(title = "Goverment Fund",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(valueBoxOutput("Min_",width = 3)),
              fluidRow(box(background = "black",width=3,title='Top 10 Highest Goverment Fund In India(In Rupees)',tableOutput("donationtable")),
                       box(background = "black",width=9,title='Goverment Fund',plotlyOutput("Piefund")))),
              #fluidRow(column(width=6,d3Output("top_donation")))),
      #diseases
      tabItem(tabName = "diseases",
              fluidRow(box(title = "Diseases",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(box(background = "black",
                           solidHeader = TRUE,
                           title = "Select Disease",
                           collapsible = TRUE,status = "success",selectInput('diseaseid',"Diseases","")),valueBoxOutput("diseasevalue",width = 3),valueBoxOutput("diseasevaluepuppy",width = 3)
                       ),
              fluidRow(box(background = "black",width=12,plotlyOutput("plot2")))
          ),
      #gender
      tabItem(tabName = "Gender",
              fluidRow(box(title = "Sex Ratio",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(tabBox(width = 500,#title = tagList(shiny::icon("gear"), "tabBox status"),
                tabPanel(title="Male",fluidRow(box(background = "black",width=12,plotlyOutput("view_male")))),
                tabPanel(title="FeMale",fluidRow(box(background = "black",width=12,plotlyOutput("view_female"))))
              ))),
      #status
      tabItem(tabName = "status",
              fluidRow(box(title = "Status",height = 50,width = 12,solidHeader = TRUE,background = "black")),
              #selectInput('deathyrid',"Select year ",""),
              fluidRow(box(background = "black",
                           solidHeader = TRUE,
                           title = "Select Status",
                           collapsible = TRUE,status = "success",selectInput('statusid',"Status ","")),valueBoxOutput("statusvalue",width = 3),valueBoxOutput("statusvaluepuppy",width = 3)
                       ),
              fluidRow(box(background = "black",width=12,plotlyOutput("statusplot")))
             # fluidRow(box(selectInput('genderid',"Select Gender ","")))
              ),
      
       
       #linear
      tabItem(tabName = "all",
              fluidRow(box(title = "Linear Graph for Status",height = 50,width = 12,solidHeader = TRUE,background = "black")),
              fluidRow(tabBox(width = 500,
                              tabPanel(title="Status",fluidRow(box(background = "black",width=12,plotOutput("statusid"))),
                                       br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summarystatus"))))
                              # tabPanel(title="Not Found",fluidRow(plotlyOutput("not")),verbatimTextOutput("summarynot"),verbatimTextOutput("predictnot")),
                              # tabPanel(title="Admitted",fluidRow(plotOutput("admit")),verbatimTextOutput("summaryadmit")),
                              # tabPanel(title="Released",fluidRow(plotlyOutput("released")),verbatimTextOutput("summaryreleased")),
                              # tabPanel(title="Dead",fluidRow(plotlyOutput("dead")),verbatimTextOutput("summarydead"))
              ))
      ),
       tabItem(tabName = "dog",
               
               fluidRow(tabBox(width = 500,
                              # tabPanel(title="Status",fluidRow(plotOutput("statusid")),verbatimTextOutput("summarystatus")),
                               tabPanel(title="Not Found",fluidRow(box(background = "black",width=12,plotlyOutput("notdog"))),
                                        br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summarynotdog")),box(title = "Prediction",status = "success",background = "black",verbatimTextOutput("predictnotdog")))),
                               tabPanel(title="Admitted",fluidRow(box(background = "black",width=12,plotOutput("admitdog"))),
                                        br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summaryadmitdog")))),
                               tabPanel(title="Released",fluidRow(box(background = "black",width=12,plotlyOutput("releaseddog"))),
                                        br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summaryreleaseddog")))),
                               tabPanel(title="Dead",fluidRow(box(background = "black",width=12,plotlyOutput("deaddog"))),
                                        br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summarydeaddog"))))
               ))
               ),
      tabItem(tabName = "puppy",
              fluidRow(tabBox(width = 500,
                              #tabPanel(title="Status",fluidRow(plotOutput("statusid")),verbatimTextOutput("summarystatus")),
                              tabPanel(title="Not Found",fluidRow(box(background = "black",width=12,plotlyOutput("notpuppy"))),
                                       br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summarynotpuppy")),box(title = "Prediction",status = "success",background = "black",verbatimTextOutput("predictnotpuppy")))),
                              tabPanel(title="Admitted",fluidRow(box(background = "black",width=12,plotOutput("admitpuppy"))),
                                       br(),fluidRow(box(title = "Summary",status = "success",background = "black", verbatimTextOutput("summaryadmitpuppy")))),
                              tabPanel(title="Released",fluidRow(box(background = "black",width=12,plotlyOutput("releasedpuppy"))),
                                       br(),fluidRow(box(title = "Summary",status = "success",background = "black", verbatimTextOutput("summaryreleasedpuppy")))),
                              tabPanel(title="Dead",fluidRow(box(background = "black",width=12,plotlyOutput("deadpuppy"))),
                                       br(),fluidRow(box(title = "Summary",status = "success",background = "black", verbatimTextOutput("summarydeadpuppy"))))
              ))
      ),
      tabItem(tabName = "dogdisease",
              tabBox(width = 500,
                     tabPanel(title="Dog",fluidRow(box(background = "black",width=12,plotOutput("dogdiseases"))),
                              br(),fluidRow(box(title = "Summary",status = "success",background = "black",verbatimTextOutput("summarydiseasedog"))))
                     #tabPanel(title="puppy",fluidRow(plotlyOutput("pie")))
              )),
      
      #pie
      tabItem(tabName = "diseasepie",
              fluidRow(box(title = "Pie chart",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(box(title = "select Year",solidHeader = TRUE,collapsible = TRUE,background = "black",status = "success",
                           selectInput('pieid',"Year ",""))),
              tabBox(width = 20,
              tabPanel(title="Dog",fluidRow(box(background = "black",width=12,plotlyOutput("piedog")))),
              tabPanel(title="puppy",fluidRow(box(background = "black",width=12,plotlyOutput("pie"))))
              )),
      
      tabItem(tabName = "statuspie",
              fluidRow(box(title = "Pie chart",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(box(title = "select Year",solidHeader = TRUE,collapsible = TRUE,background = "black",status = "success"
                           ,selectInput('piestatusyr',"Year ","")),box(title = "select Status",solidHeader = TRUE,collapsible = TRUE,background = "black",status = "success",
                                                                 selectInput('statuspieid',"Status",""))),
              tabBox(width = 20,
                     tabPanel(title="Dog",fluidRow(plotlyOutput("statuspiedog"))),
                     tabPanel(title="puppy",fluidRow(plotlyOutput("statuspuppypie")))
              )),
      
      
      #cluster
      tabItem(tabName = "cluster",
              fluidRow(box(title = "Cluster",height = 50,width = 12,background = "black",solidHeader = TRUE)),
              fluidRow(box(background = "black",width=12,plotOutput("cluster"))),
              fluidRow(box(dataTableOutput("clustertabel")))
      )
              
              
      
),
      
    
            
    
    #header (dog care)
    tags$head(tags$style(HTML('
       .main-header .logo {
         font-family: "Georgia", Times, "Times New Roman", serif;
         font-weight: bold;
         font-size: 24px;
         
       }
     ')))
  
)
)