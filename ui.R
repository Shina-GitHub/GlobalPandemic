###################################
##### Pandemic Website - ui.R #####
###################################
library(shiny) # load shiny at beginning at both scripts
library(shinycssloaders)
library(shinyjs)
#load data
library(leaflet)
countryData = read.csv("HospitalBedPopuRscript.csv")
shinyUI(fluidPage( 
  useShinyjs(),
  titlePanel("Global Pandemic"),
  tabsetPanel(
        tabPanel(id ="Ebola",title = strong("EBOLA Model"),
             sidebarLayout(
               sidebarPanel(
                 #withMathJax(),
                 #HTML("Infection rate(per day) &beta;:")
                 numericInput("R0",label = HTML("Basic Reproduction Number: R<sub>0</sub>"),NULL),
                 sliderInput("ebolaInfecRate", label = HTML("Infection Rate(per day): &beta;<sub>1</sub>") , 
                             min=0, max=5, value=0.35,step = 0.01),
                 sliderInput("ebolaCaseAs", label= HTML("Case Ascertianment: &rho;"), 
                             min=0, max=1, value=0.5, step = 0.01),
                 sliderInput("ebolaCaseFatHosp",label= HTML("Hospital Case Fatality (per day): c<sub>h</sub>"), 
                             min=0, max=1, value=0.35, step = 0.01),
                 sliderInput("ebolaHospcap", label= HTML(" Number of Isolation Beds: K"), 
                             min=1000, max=250000, value=50000, step = 1000),
                 sliderInput("ebolaBur", label= HTML("Burial Period (days): 1/&tau;"), 
                             min=1, max=50, value=3, step = 1),
                 sliderInput("ebolaPrev", "Initial Prevalence (Number of people):", 
                             min=0, max=50000, value=3000, step = 1),
                 sliderInput("ebolaTime", "Scenario Period (days):", 
                             min=1, max=365, value=63, step = 1, animate = TRUE),
                 # animationOptions(interval = 1000, loop = TRUE, playButton = NULL,
                 #                  pauseButton = NULL),
                 selectInput(inputId = "ebolaSouCoun", label = strong("Source Country:"),
                             choices = unique(countryData$Country), selected = "DR Congo"),
                 selectInput(inputId = "ebolaWatCoun", label = strong("Watch Country:"),
                             choices = unique(countryData$Country),
                             selected = "Australia")),

               mainPanel(
                 tabsetPanel(position = "right",
                    tabPanel("Model Description", includeMarkdown('./aboutmodel.md')),
                    tabPanel("Pandemic Map", h3("Spatial Distribution of Ebola"),
                             absolutePanel(selectInput("indicator_map", NULL,
                             list("Regions"=c("Africa"= "Africa_Cases",
                                              "Asia" = "Asia_cases",
                                              "Asia-Pacific"= "Asia_Pacific_cases",
                                              "Australia and Oceania"= "Australia_cases",
                                              "Europe"="Europe_cases",
                                              "Global" = "All_cases",
                                              "North America"= "North_America_cases",
                                              "South America"="South_America_cases")),
                             selected = "All_cases"),top='10%', right='3%', width='250px', height='200px'),
                             leafletOutput("mapebolaDisplay", height =600)%>% withSpinner(),
                             HTML('<em>The depiction and use of boundaries, geographic names 
                                  and related data shown on maps and included in lists, 
                                  tables, documents, and databases on this website are not
                                  warranted to be error free nor do they necessarily imply 
                                  official endorsement or acceptance by Infectious Modelling 
                                  Group, Australian Institute of Tropical Health and Medicine, 
                                  James Cook University,Townsville, Australia.</em>'),
                             HTML('<br><br><br>')),
                    
                    tabPanel("Global Prevalence",h3("Global Pandemic Predictions:"),
                             plotOutput("ebolaDisGraph",height=500)%>% withSpinner(),
                             HTML('<em> While the authors have attempted to ensure that results 
                            in these graphs are accurate, the authors assume no legal liability 
                            or responsibility for any errors or omissions in the information or 
                            any loss or damage resulting from using the results on this page.</em>'),
                             HTML('<br><br><br>')),
                    tabPanel("Source Country",h3("Source Country Pandemic Predictions:"),
                             plotOutput("ebolasourcDisGraph",height = 500)%>% withSpinner(),
                             HTML('<em> While the authors have attempted to ensure that results 
                            in these graphs are accurate, the authors assume no legal liability 
                            or responsibility for any errors or omissions in the information or 
                            any loss or damage resulting from using the results on this page.</em>'),
                             HTML('<br><br><br>')),
                    tabPanel("Watch Country",h3("Watch Country Pandemic Predictions:"),
                             plotOutput("ebolawatDisGraph", height = 500)%>% withSpinner(), 
                             HTML('<em> While the authors have attempted to ensure that results 
                             in these graphs are accurate, the authors assume no legal liability 
                             or responsibility for any errors or omissions in the information or 
                             any loss or damage resulting from using the results on this page.</em>'),
                             HTML('<br><br><br>'))
                         )))
    ),
    tabPanel(id ="EbolaInter",title = strong("Intervention Strategies"),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "ebolaSouCounInterv", label = strong("Source Country:"),
                             choices = unique(countryData$Country),selected = "DR Congo"),
                 selectInput(inputId = "ebolaWatCounInterv", label = strong("Watch Country:"),
                             choices = unique(countryData$Country),
                             selected = "Australia"),
                 radioButtons("CountryInter", "Choose Country(ies) to apply intervention:",
                                    choiceNames =
                                      list("Source Country Only","Watch Country Only",
                                           "Source and Watch Countries","All Countries"),
                                    choiceValues =
                                      list("source", "watch", "both", "all"),
                              selected = "source"),
                 sliderInput("ebolaTimeInterv", "Start Time (days):", 
                             min=1, max=50, value=5,step = 1, animate = TRUE),
                 numericInput("R0Interv",label = HTML("Basic Reproduction Number: R<sub>0</sub>"),NULL),
                 sliderInput("ebolaInfecRateInterv", label = HTML("Infection Rate(per day): &beta;<sub>1</sub>"), 
                             min=0, max=5, value=0.25,step = 0.01),
                 sliderInput("ebolaascerInterv", label= HTML("Case Ascertianment: &rho;"), 
                             min=0, max=1, value=0.5, step = 0.01),
                 sliderInput("ebolaHospInterv", label= HTML(" Number of Isolation Beds: K"), 
                             min=1000, max=250000, value=50000, step = 1000),
                 sliderInput("ebolaTravelBan", "Travel Ban Fraction", 
                           min=0, max=1, value=0.5, step = 0.01)),
               mainPanel(Right = 20,
                 tabsetPanel(position = "right",
                    tabPanel("Pandemic Map",h4("Spatial Distribution of Ebola Virus with 
                                               Intervention"),
                             leafletOutput("mapebolaDisplayInterv", height =400)%>% withSpinner(),
                             h4("Spatial Distribution of Ebola Virus without Intervention"),
                             absolutePanel(selectInput("indicator_map1", NULL,
                             list("Regions"=c("Africa"= "Africa_Cases",
                                  "Asia" = "Asia_cases",
                                  "Asia-Pacific"= "Asia_Pacific_cases",
                                  "Australia and Oceania"= "Australia_cases",
                                  "Europe"="Europe_cases",
                                  "Global" = "All_cases",
                                  "North America"= "North_America_cases",
                                  "South America"="South_America_cases")),
                                  selected = "All_cases"),top='10%', right='3%', width='250px', height='200px'),
                             leafletOutput("mapebolaDisplay1", height =400)%>% withSpinner(),
                             HTML('<em>The depiction and use of boundaries, geographic names 
                                  and related data shown on maps and included in lists, 
                                  tables, documents, and databases on this website are not
                                  warranted to be error free nor do they necessarily imply 
                                  official endorsement or acceptance by Infectious Modelling 
                                  Group, Australian Institute of Tropical Health and Medicine, 
                                  James Cook University,Townsville, Australia.</em>'),
                             HTML('<br><br><br>')),
                    tabPanel("Global Prevalence",h4("Global Pandemic Predictions for 
                            Different Intervention Scenarios:"),
                             plotOutput("ebolaDisGraphInterv",height=500)%>% withSpinner(),
                             HTML('<em> While the authors have attempted to ensure that results 
                             in these graphs are accurate, the authors assume no legal liability 
                             or responsibility for any errors or omissions in the information or 
                             any loss or damage resulting from using the results on this page.</em>'),
                             HTML('<br><br><br>')),
                    tabPanel("Source Country",h4("Source Country Pandemic Predictions for 
                            Different Intervention Scenarios:"),
                             plotOutput("ebolasourcDisGraphInterv", height = 500)%>% withSpinner(),
                             HTML('<em> While the authors have attempted to ensure that results 
                             in these graphs are accurate, the authors assume no legal liability 
                             or responsibility for any errors or omissions in the information or 
                             any loss or damage resulting from using the results on this page.</em>'),
                             HTML('<br><br><br>')),
                    tabPanel("Watch Country",h4("Watch Country Pandemic Predictions for 
                            Different Intervention Scenarios:"),
                             plotOutput("ebolawatDisGraphInterv", height = 500)%>% withSpinner(),
                             HTML('<em> While the authors have attempted to ensure that results 
                             in these graphs are accurate, the authors assume no legal liability 
                                  or responsibility for any errors or omissions in the information or 
                                  any loss or damage resulting from using the results on this page.</em>'),
                             HTML('<br><br><br>'))
                )))
    )
  )
)) 
