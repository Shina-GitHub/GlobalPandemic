library(shiny)
library(plyr)
#library(dplyr)
#library(tidyr)
library(ggplot2)
library(tidyverse)
require(leaflet)
#library(leafletR)
#library("leaflet", lib.loc="~/R/win-library/3.4")
library(ggthemes)
library(maps)
library(gridExtra)
#library(rgdal)
library(rworldmap)
require(scales)
require(Rcpp)
require(cowplot)
library(RColorBrewer)
#Data description
# migrationMain.csv is the dataset showing migration rate by one country to another. 
# We have adjusted for adjacent countries
# HospitalBedPopuRscript.csv contains list of countries and their number of hospital beds.
sourceCpp("rowMaxRcpp.cpp")
source("rungeK_Ebola.R")
migrationData= read.csv("migrationMain1.csv") #new calculation- remove 1 and get old cal
dataCouInfo1 = read.csv("HospitalBedPopuRscript.csv") 
#Reading countires in a continent:
africaData =read.csv("Africa.csv")
asiaData = read.csv("Asia.csv")
asiaPacificData = read.csv("Asia_Pacific.csv")
australiaOceaniaData = read.csv("AustraliaOceania.csv")
europeData = read.csv("Europe.csv")
northAmericaData = read.csv("NorthAmerica.csv")
southAmericaData = read.csv("SouthAmerica.csv")

##############################
deathRate = dataCouInfo1$DeathrateWorld
#############Ebola model
migration = data.matrix(migrationData[1:length(migrationData[,1]), 
                               2:(length(migrationData[,1])+1)])
# ####Convert Migration to Number
# countryPopulation = dataCouInfo1$PopulationWorldData
migration = round(migration)
diag(migration)<-0
#################Ebola Solver
#######################################################
shinyServer(function(input, output, session) {
#SIS Non reactive parameter


##########Ebola model and reactive
observe({
  val11<- input$ebolaSouCoun
  val21<-  input$ebolaWatCoun
  updateSelectInput(session, inputId = "ebolaSouCounInterv", selected= val11)
  updateSelectInput(session, inputId = "ebolaWatCounInterv", selected= val21)
})
observe({
  b0 = 0.1
  b2 = 0.02
  b3 = 0.2
  lat1 = 0.047
  preDet = 0.25
  postDet = 0.16
  caseFat = 0.7
  b1 = input$ebolaInfecRate
  rho = input$ebolaCaseAs
  tau = 1/input$ebolaBur
  cfrh = input$ebolaCaseFatHosp
  val = (b1/preDet) + ((b0*(1-rho)+b2*rho)/postDet)+ (b3*(cfrh*(1-rho)+rho*caseFat)/tau)
    updateNumericInput(session, "R0", value = val)
    disable("R0")
  })
observe({
  b0 = 0.1
  b2 = 0.02
  b3 = 0.2
  lat1 = 0.047
  preDet = 0.25
  postDet = 0.16
  caseFat = 0.7
  b1 = input$ebolaInfecRateInterv
  rho = input$ebolaascerInterv
  tau = 1/input$ebolaBur
  cfrh = input$ebolaCaseFatHosp
  val = (b1/preDet) + ((b0*(1-rho)+b2*rho)/postDet)+ (b3*(cfrh*(1-rho)+rho*caseFat)/tau)
  updateNumericInput(session, "R0Interv", value = val)
  disable("R0Interv")
})
dataebola =reactive({
 
  ebolaSourcCount <- c(0,0) # One for Country and One for prevalemce
  ebolaSourcCount[1] <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaSouCoun]
  ebolaSourcCount[2] <- input$ebolaPrev
  
  # dataEBOLA <-EBOLA.modelEulerM(input$ebolaTime,input$ebolaInfecRate, input$ebolaCaseAs
  #                   ,input$ebolaCaseFatHosp,input$ebolaBur,input$ebolaHospcap,deathRate,
  #                   200,migration,dataCouInfo1,ebolaSourcCount) 
  dataEBOLA <-EbolaRunModel(input$ebolaTime,input$ebolaInfecRate, input$ebolaCaseAs
                            ,input$ebolaCaseFatHosp,input$ebolaBur,
                            input$ebolaHospcap,deathRate,
                            200,migration,dataCouInfo1,ebolaSourcCount,
                            0,0,0,0,1,0,0,0,0,0,0)

 #1 is to show no intervention 
})
####Ebola Pandemic Map
#### Number of country is 200

output$mapebolaDisplay <-  renderLeaflet({
  ebolaSpaTempData = dataCouInfo1
  timep =dataebola()$Dyna[,1]
  ebolaSpaTempData$Prevalence = dataebola()$Dyna[length(timep),(200*3+2):(200*4+1)]  #showing num infected at last time
  #Getting prevalence by data
  if (input$indicator_map == "Africa_Cases"){
     africaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% africaData$ISO3)
     
     africaData$Prevalence = africaDataMap$Prevalence
     myMapPlot = africaData
  }
  else if(input$indicator_map == "Asia_cases"){
    asiaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% asiaData$ISO3)
    asiaData$Prevalence = asiaDataMap$Prevalence
    myMapPlot = asiaData
  }
  else if(input$indicator_map == "Asia_Pacific_cases"){
    asiaPacificDataMap = ebolaSpaTempData %>% filter(ISO3 %in% asiaPacificData$ISO3)
    asiaPacificData$Prevalence = asiaPacificDataMap$Prevalence
    myMapPlot = asiaPacificData
  }
  else if(input$indicator_map == "Australia_cases"){
    australiaOceaniaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% australiaOceaniaData$ISO3)
    australiaOceaniaData$Prevalence = australiaOceaniaDataMap$Prevalence
    myMapPlot = australiaOceaniaData
  }
  else if(input$indicator_map == "Europe_cases"){
    europeDataMap = ebolaSpaTempData %>% filter(ISO3 %in% europeData$ISO3)
    europeData$Prevalence = europeDataMap$Prevalence
    myMapPlot = europeData
  }
  else if(input$indicator_map == "All_cases"){
    myMapPlot = ebolaSpaTempData
  }
  else if(input$indicator_map == "North_America_cases"){
    northAmericaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% northAmericaData$ISO3)
    northAmericaData$Prevalence = northAmericaDataMap$Prevalence
    myMapPlot = northAmericaData
  }
  else if(input$indicator_map == "South_America_cases"){
    southAmericaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% southAmericaData$ISO3)
    southAmericaData$Prevalence = southAmericaDataMap$Prevalence
    myMapPlot = southAmericaData
  }
  
  EBOLAsPDF <- joinCountryData2Map(myMapPlot
                                  ,joinCode = "ISO3"
                                  ,nameJoinColumn = "ISO3") 
  # qpal <- colorBin(rev(viridis::viridis(5)),
  #                       SISsPDF$Prevalence, bins=5)
  #c("#008080","#800080", "#00FF00", "#FF0000")
  # qpal = colorBin(palette = "Reds", pretty = TRUE,
  #                 domain =EBOLAsPDF$Prevalence,bins=6)
  bins=c(0,1000,10000,100000,1000000,10000000,100000000,1000000000)
  qpal = colorBin(palette = "Reds", pretty = TRUE,
                  domain =EBOLAsPDF$Prevalence,bins=bins)
  # bins <- c(0, 1, 4,10)
  # qpal = colorBin("Blues", pretty = TRUE, domain =EBOLAsPDF$Prevalence,bins=bins)
   l <- leaflet(EBOLAsPDF, options =
                 leafletOptions(attributionControl = FALSE, minzoom=1)) %>%
    addPolygons(
      label=~stringr::str_c(
        NAME, ' ',
        formatC(Prevalence, big.mark = ',', format='d')),
      labelOptions= labelOptions(direction = 'auto'),
      weight=1,color='#333333', opacity=1,
      fillColor = ~qpal(Prevalence), fillOpacity = 1,
      highlightOptions = highlightOptions(
        color='#000000', weight = 2,
        bringToFront = TRUE, sendToBack = TRUE)
    ) %>%
    addLegend(position="bottomleft", pal = qpal, values = ~Prevalence,
      title = htmltools::HTML("Population Prevalence"),
      opacity = 1 )
  
})
#############################EbolaGlobalPrevalence
output$ebolaDisGraph <- renderPlot({
 
  #print(dim(dataebola$Dyna))
  # generate an rnorm distribution and plot it
  theebolaSour <-input$ebolaSouCoun

  # print(max(rowSums(datasir$Inci)))
  nmea =length(dataebola()$Dyna[,1])
  ebolaoutglo = data.frame(matrix(nrow = nmea, ncol = 3))
  
  names(ebolaoutglo) = c("Time","Prev","Cum")
  
  ebolaoutglo$Time =  dataebola()$Dyna[,1]
  #print( seiroutwatch)
  ebolaoutglo$Prev = rowSums(dataebola()$Dyna[,(200*3+2):(200*4+1)])
 
  ebolaoutglo$Cum = rowSums(dataebola()$Inci)
  p1 = ggplot( ebolaoutglo, aes(x=Time,Prev))+geom_line(size = 1.8, colour ="red")+
    ylim(0,max(ebolaoutglo$Prev))+labs(x = "Days", y = "Number of Infected Individuals")+
    ggtitle("Global Prevalence")+theme(axis.text.x = element_text(size = 20),
                          axis.text.y = element_text(size = 20),
                          axis.title.x = element_text(size = 20),
                            axis.title.y = element_text(size = 20),
                          plot.title = element_text(size= 20, face = "bold"),
                        plot.background = element_rect(fill="aliceblue", color = NA),
                        panel.background = element_rect(fill = "white",colour = NA),
                        panel.grid.major  = element_line(color = "gray91"))

 
  p2 = ggplot( ebolaoutglo, aes(x=Time,Cum))+geom_line(size = 1.8, colour ="red")+
    ylim(0,max( ebolaoutglo$Cum))+
    labs(x = "Days", y="")+
    ggtitle("Global Cumulative Incidence")+theme(axis.text.x = element_text(size = 20),
                                axis.text.y = element_text(size = 20),
                                axis.title.x = element_text(size = 20),
                                axis.title.y = element_text(size = 20),
                                plot.title = element_text(size= 20, face = "bold"),
                          plot.background = element_rect(fill="aliceblue", color = NA),
                          panel.background = element_rect(fill = "white",colour = NA),
                          panel.grid.major  = element_line(color = "gray91"))
   grid.arrange(p1,p2, nrow =1)
  # g2 <- cowplot::ggdraw(g) + 
  #   theme(plot.background = element_rect(fill="wheat1", color = NA))
  # plot(g2) # Not needed
})

#######EbolaWatch
output$ebolawatDisGraph <- renderPlot({
  ebolaWatchCount <- c(0)
  ebolaWatchCount[1] <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaWatCoun]
  
  #print(dim(dataebola$Dyna))
  
  theebolaSour <-input$ebolaSouCoun
  #print(max(rowSums(datasir$Inci)))
  #print(length(datasir$Dyna[,1]))
  #print(dim(datasir$Inci))
  nmea =length(dataebola()$Dyna[,1])
  ebolaoutwatch = data.frame(matrix(nrow = nmea, ncol = 3))
  
  names(ebolaoutwatch) = c("Time","Prev","Cum")
  
  ebolaoutwatch$Time =  dataebola()$Dyna[,1]
  #print( seiroutwatch)
  ebolaoutwatch$Prev = dataebola()$Dyna[,(200*3+1+ebolaWatchCount)]
  ebolaoutwatch$Cum = dataebola()$Inci[,ebolaWatchCount]
  p1 = ggplot(ebolaoutwatch, aes(x=Time,Prev))+geom_line(size = 1.8, colour ="red")+
    ylim(0,max(ebolaoutwatch$Prev))+labs(x = "Days", y = "Number of Infected Individuals")+
    ggtitle("Prevalence")+theme(axis.text.x = element_text(size = 20),
                                axis.text.y = element_text(size = 20),
                                axis.title.x = element_text(size = 20),
                                axis.title.y = element_text(size = 20),
                                plot.title = element_text(size= 20, face = "bold"),
                                plot.background = element_rect(fill="aliceblue", color = NA),
                                panel.background = element_rect(fill = "white",colour = NA),
                                panel.grid.major  = element_line(color = "gray91"))
  p2 = ggplot(ebolaoutwatch, aes(x=Time,Cum))+geom_line(size = 1.8, colour ="red")+
    ylim(0,max(ebolaoutwatch$Cum))+
    labs(x = "Days", y= "")+
    ggtitle("Cumulative Incidence")+theme(axis.text.x = element_text(size = 20),
                                  axis.text.y = element_text(size = 20),
                                  axis.title.x = element_text(size = 20),
                                  axis.title.y = element_text(size = 20),
                                  plot.title = element_text(size= 20, face = "bold"),
                                  plot.background = element_rect(fill="aliceblue", color = NA),
                                  panel.background = element_rect(fill = "white",colour = NA),
                                 panel.grid.major  = element_line(color = "gray91"))
grid.arrange(p1,p2, nrow =1)
})
#######EbolaSource
output$ebolasourcDisGraph <- renderPlot({
  ebolaSourceCount <- c(0)
  ebolaSourceCount[1] <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaSouCoun]
  
  #print(dim(dataebola$Dyna))
  
  theebolaSour <-input$ebolaSouCoun
  #print(max(rowSums(datasir$Inci)))
  #print(length(datasir$Dyna[,1]))
  #print(dim(datasir$Inci))
  nmea =length(dataebola()$Dyna[,1])
  ebolaoutsource = data.frame(matrix(nrow = nmea, ncol = 3))
  
  names(ebolaoutsource) = c("Time","Prev","Cum")
  
  ebolaoutsource$Time =  dataebola()$Dyna[,1]
  #print( seiroutwatch)
  ebolaoutsource$Prev = dataebola()$Dyna[,(200*3+1+ebolaSourceCount)]
  ebolaoutsource$Cum = dataebola()$Inci[,ebolaSourceCount]
  p1 = ggplot(ebolaoutsource, aes(x=Time,Prev))+geom_line(size = 1.8, colour ="red")+
    ylim(0,max(ebolaoutsource$Prev))+labs(x = "Days", y = "Number of Infected Individuals")+
    ggtitle("Prevalence")+theme(axis.text.x = element_text(size = 20),
                                axis.text.y = element_text(size = 20),
                                axis.title.x = element_text(size = 20),
                                axis.title.y = element_text(size = 20),
                                plot.title = element_text(size= 20, face = "bold"),
                                plot.background = element_rect(fill="aliceblue", color = NA),
                                panel.background = element_rect(fill = "white",colour = NA),
                                panel.grid.major  = element_line(color = "gray91"))
  p2 = ggplot(ebolaoutsource, aes(x=Time,Cum))+geom_line(size = 1.8, colour ="red")+
    ylim(0,max(ebolaoutsource$Cum))+
    labs(x = "Days", y= "")+
    ggtitle("Cumulative Incidence")+theme(axis.text.x = element_text(size = 20),
                                    axis.text.y = element_text(size = 20),
                                    axis.title.x = element_text(size = 20),
                                    axis.title.y = element_text(size = 20),
                                    plot.title = element_text(size= 20, face = "bold"),
                                    plot.background = element_rect(fill="aliceblue", color = NA),
                                    panel.background = element_rect(fill = "white",colour = NA),
                                    panel.grid.major  = element_line(color = "gray91"))
  grid.arrange(p1,p2, nrow =1)
})
#####
##########Ebola model and reactive
observe({
  val1<- input$ebolaSouCounInterv
  val2<-  input$ebolaWatCounInterv
  updateSelectInput(session, inputId = "ebolaSouCoun", selected= val1)
  updateSelectInput(session, inputId = "ebolaWatCoun", selected= val2)
})
#Simulation Intervention
dataebolaInterv =reactive({
  indexWatch <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaWatCoun]
  indexSource <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaSouCoun]
  indicator = 2 #for intervention
  startinter = input$ebolaTimeInterv
  newInitial =  dataebola()$Dyna[startinter,2:((200*8)+1)]# 8 num of states, 200 num of coun
  newCumInitial = dataebola()$Inci[startinter,]
  if (input$CountryInter == "source"){
    if (input$ebolaTimeInterv < input$ebolaTime){
    dataEBOLAInter <-EbolaRunModel(input$ebolaTime,input$ebolaInfecRate, 
                                   input$ebolaCaseAs,input$ebolaCaseFatHosp,input$ebolaBur,
                                   input$ebolaHospcap,deathRate,
                                   200,migration,dataCouInfo1,ebolaSourcCount,
                                   startinter,newInitial,newCumInitial,input$ebolaTravelBan, 2,1,
                                   input$ebolaInfecRateInterv,input$ebolaascerInterv,input$ebolaHospInterv,
                                   indexSource, indexWatch)
    
    }
    else{}
  }
  else if (input$CountryInter == "watch"){
    if (input$ebolaTimeInterv < input$ebolaTime){
    dataEBOLAInter <-EbolaRunModel(input$ebolaTime,input$ebolaInfecRate, 
                                   input$ebolaCaseAs,input$ebolaCaseFatHosp,input$ebolaBur,
                                   input$ebolaHospcap,deathRate,
                                   200,migration,dataCouInfo1,ebolaSourcCount,
                                   startinter,newInitial,newCumInitial,input$ebolaTravelBan, 2,2,
                                   input$ebolaInfecRateInterv,input$ebolaascerInterv,input$ebolaHospInterv,
                                   indexSource, indexWatch)
    }
    else{}
  }
  else if (input$CountryInter == "both"){
    if (input$ebolaTimeInterv < input$ebolaTime){
    dataEBOLAInter <-EbolaRunModel(input$ebolaTime,input$ebolaInfecRate, 
                                   input$ebolaCaseAs,input$ebolaCaseFatHosp,input$ebolaBur,
                                   input$ebolaHospcap,deathRate,
                                   200,migration,dataCouInfo1,ebolaSourcCount,
                                   startinter,newInitial,newCumInitial,input$ebolaTravelBan, 2,3,
                                   input$ebolaInfecRateInterv,input$ebolaascerInterv,input$ebolaHospInterv,
                                   indexSource, indexWatch)
    }
    else {}
  }
  else if (input$CountryInter == "all"){
      if (input$ebolaTimeInterv < input$ebolaTime){#check whether start time inter
        #is less than final time in the main model
        #
        # input$ebolaSouCounInterv = input$ebolaSouCoun
        # input$ebolaWatCounInterv = input$ebolaWatCoun
        #print(newCumInitial)
        # dataEBOLA <-EBOLA.modelEulerM(input$ebolaTime,input$ebolaInfecRate, input$ebolaCaseAs
    #                   ,input$ebolaCaseFatHosp,input$ebolaBur,input$ebolaHospcap,deathRate,
    #                   200,migration,dataCouInfo1,ebolaSourcCount) 
     dataEBOLAInter <-EbolaRunModel(input$ebolaTime,input$ebolaInfecRateInterv, 
                                   input$ebolaascerInterv,input$ebolaCaseFatHosp,input$ebolaBur,
                                   input$ebolaHospInterv,deathRate,
                                   200,migration,dataCouInfo1,ebolaSourcCount,
                                   startinter,newInitial,newCumInitial,input$ebolaTravelBan, 2,4,0,0,0,0,0)
      }
    else {}
 }
})
####Ebola Pandemic with Intervention
output$mapebolaDisplayInterv <-  renderLeaflet({
  ebolaSpaTempData = dataCouInfo1
  timep =dataebolaInterv()$Dyna[,1]
  ebolaSpaTempData$Prevalence = dataebolaInterv()$Dyna[length(timep),(200*3+2):(200*4+1)]  #showing num infected at last time
  #Getting prevalence by data
  if (input$indicator_map1 == "Africa_Cases"){
    africaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% africaData$ISO3)
    
    africaData$Prevalence = africaDataMap$Prevalence
    myMapPlot = africaData
  }
  else if(input$indicator_map1 == "Asia_cases"){
    asiaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% asiaData$ISO3)
    asiaData$Prevalence = asiaDataMap$Prevalence
    myMapPlot = asiaData
  }
  else if(input$indicator_map1 == "Asia_Pacific_cases"){
    asiaPacificDataMap = ebolaSpaTempData %>% filter(ISO3 %in% asiaPacificData$ISO3)
    asiaPacificData$Prevalence = asiaPacificDataMap$Prevalence
    myMapPlot = asiaPacificData
  }
  else if(input$indicator_map1 == "Australia_cases"){
    australiaOceaniaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% australiaOceaniaData$ISO3)
    australiaOceaniaData$Prevalence = australiaOceaniaDataMap$Prevalence
    myMapPlot = australiaOceaniaData
  }
  else if(input$indicator_map1 == "Europe_cases"){
    europeDataMap = ebolaSpaTempData %>% filter(ISO3 %in% europeData$ISO3)
    europeData$Prevalence = europeDataMap$Prevalence
    myMapPlot = europeData
  }
  else if(input$indicator_map1 == "All_cases"){
    myMapPlot = ebolaSpaTempData
  }
  else if(input$indicator_map1 == "North_America_cases"){
    northAmericaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% northAmericaData$ISO3)
    northAmericaData$Prevalence = northAmericaDataMap$Prevalence
    myMapPlot = northAmericaData
  }
  else if(input$indicator_map1 == "South_America_cases"){
    southAmericaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% southAmericaData$ISO3)
    southAmericaData$Prevalence = southAmericaDataMap$Prevalence
    myMapPlot = southAmericaData
  }
  
  EBOLAsPDF <- joinCountryData2Map(myMapPlot
                                   ,joinCode = "ISO3"
                                   ,nameJoinColumn = "ISO3") 
  # qpal <- colorBin(rev(viridis::viridis(5)),
  #                       SISsPDF$Prevalence, bins=5)
  #c("#008080","#800080", "#00FF00", "#FF0000")
  # qpal = colorBin(palette = "Reds", pretty = TRUE,
  #                 domain =EBOLAsPDF$Prevalence,bins=6)
  bins=c(0,1000,10000,100000,1000000,10000000,100000000,1000000000)
  qpal = colorBin(palette = "Reds", pretty = TRUE,
                  domain =EBOLAsPDF$Prevalence,bins=bins)
  # bins <- c(0, 1, 4,10)
  # qpal = colorBin("Blues", pretty = TRUE, domain =EBOLAsPDF$Prevalence,bins=bins)
  l <- leaflet(EBOLAsPDF, options =
                 leafletOptions(attributionControl = FALSE, minzoom=1)) %>%
    addPolygons(
      label=~stringr::str_c(
        NAME, ' ',
        formatC(Prevalence, big.mark = ',', format='d')),
      labelOptions= labelOptions(direction = 'auto'),
      weight=1,color='#333333', opacity=1,
      fillColor = ~qpal(Prevalence), fillOpacity = 1,
      highlightOptions = highlightOptions(
        color='#000000', weight = 2,
        bringToFront = TRUE, sendToBack = TRUE)
    ) %>%
    addLegend(position="bottomleft", pal = qpal, values = ~Prevalence,
              title = htmltools::HTML("Population Prevalence"),
              opacity = 1 )
  
})
##Ebola Pandemic without Intervention
output$mapebolaDisplay1 <-  renderLeaflet({
  ebolaSpaTempData = dataCouInfo1
  timep =dataebola()$Dyna[,1]
  ebolaSpaTempData$Prevalence = dataebola()$Dyna[length(timep),(200*3+2):(200*4+1)]  #showing num infected at last time
  #Getting prevalence by data
  if (input$indicator_map1 == "Africa_Cases"){
    africaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% africaData$ISO3)
    
    africaData$Prevalence = africaDataMap$Prevalence
    myMapPlot = africaData
  }
  else if(input$indicator_map1 == "Asia_cases"){
    asiaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% asiaData$ISO3)
    asiaData$Prevalence = asiaDataMap$Prevalence
    myMapPlot = asiaData
  }
  else if(input$indicator_map1 == "Asia_Pacific_cases"){
    asiaPacificDataMap = ebolaSpaTempData %>% filter(ISO3 %in% asiaPacificData$ISO3)
    asiaPacificData$Prevalence = asiaPacificDataMap$Prevalence
    myMapPlot = asiaPacificData
  }
  else if(input$indicator_map1 == "Australia_cases"){
    australiaOceaniaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% australiaOceaniaData$ISO3)
    australiaOceaniaData$Prevalence = australiaOceaniaDataMap$Prevalence
    myMapPlot = australiaOceaniaData
  }
  else if(input$indicator_map1 == "Europe_cases"){
    europeDataMap = ebolaSpaTempData %>% filter(ISO3 %in% europeData$ISO3)
    europeData$Prevalence = europeDataMap$Prevalence
    myMapPlot = europeData
  }
  else if(input$indicator_map1 == "All_cases"){
    myMapPlot = ebolaSpaTempData
  }
  else if(input$indicator_map1 == "North_America_cases"){
    northAmericaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% northAmericaData$ISO3)
    northAmericaData$Prevalence = northAmericaDataMap$Prevalence
    myMapPlot = northAmericaData
  }
  else if(input$indicator_map1 == "South_America_cases"){
    southAmericaDataMap = ebolaSpaTempData %>% filter(ISO3 %in% southAmericaData$ISO3)
    southAmericaData$Prevalence = southAmericaDataMap$Prevalence
    myMapPlot = southAmericaData
  }
  
  EBOLAsPDF <- joinCountryData2Map(myMapPlot
                                   ,joinCode = "ISO3"
                                   ,nameJoinColumn = "ISO3") 
  # qpal <- colorBin(rev(viridis::viridis(5)),
  #                       SISsPDF$Prevalence, bins=5)
  #c("#008080","#800080", "#00FF00", "#FF0000")
  # qpal = colorBin(palette = "Reds", pretty = TRUE,
  #                 domain =EBOLAsPDF$Prevalence,bins=6)
  bins=c(0,1000,10000,100000,1000000,10000000,100000000,1000000000)
  qpal = colorBin(palette = "Reds", pretty = TRUE,
                  domain =EBOLAsPDF$Prevalence,bins=bins)
  # bins <- c(0, 1, 4,10)
  # qpal = colorBin("Blues", pretty = TRUE, domain =EBOLAsPDF$Prevalence,bins=bins)
  l <- leaflet(EBOLAsPDF, options =
                 leafletOptions(attributionControl = FALSE, minzoom=1)) %>%
    addPolygons(
      label=~stringr::str_c(
        NAME, ' ',
        formatC(Prevalence, big.mark = ',', format='d')),
      labelOptions= labelOptions(direction = 'auto'),
      weight=1,color='#333333', opacity=1,
      fillColor = ~qpal(Prevalence), fillOpacity = 1,
      highlightOptions = highlightOptions(
        color='#000000', weight = 2,
        bringToFront = TRUE, sendToBack = TRUE)
    ) %>%
    addLegend(position="bottomleft", pal = qpal, values = ~Prevalence,
              title = htmltools::HTML("Population Prevalence"),
              opacity = 1 )
  
})
##################################EbolaGlobalPrevalenceIntervention
output$ebolaDisGraphInterv <- renderPlot({
  #Original Plot

  theebolaSour <-input$ebolaSouCoun

  nmea =length(dataebola()$Dyna[,1])
  ebolaoutglo = data.frame(matrix(nrow = nmea, ncol = 3))
  
  names(ebolaoutglo) = c("Time","Prev","Cum")
  
  ebolaoutglo$Time =  dataebola()$Dyna[,1]
  #print( seiroutwatch)
  ebolaoutglo$Prev = rowSums(dataebola()$Dyna[,(200*3+2):(200*4+1)])
  
  ebolaoutglo$Cum = rowSums(dataebola()$Inci)
  # New output due to intervention
  nmea =length(dataebolaInterv()$Dyna[,1])
  ebolaoutglonew = data.frame(matrix(nrow = nmea, ncol = 3))
  
  names(ebolaoutglonew) = c("Timenew","Prevnew","Cumnew")
  
  ebolaoutglonew$Timenew =  dataebolaInterv()$Dyna[,1]
  
  ebolaoutglonew$Prevnew = rowSums(dataebolaInterv()$Dyna[,(200*3+2):(200*4+1)])
  
  ebolaoutglonew$Cumnew = rowSums(dataebolaInterv()$Inci)
  p1 = ggplot()+
    # geom_line(data=ebolaoutglonew, aes(x=Timenew,y=Prevnew,colour ="Intervention"),size = 1.8 )+
    # geom_line(data = ebolaoutglo, aes(x=Time,y=Prev, colour ="NO Intervention"),size = 1.8)+
    # scale_color_manual(name = "Scenarios", 
    #                    values = c("Intervention"="blue","NO Intervention"="red"))+
   geom_line(data=ebolaoutglonew, aes(x=Timenew,y=Prevnew,colour ="Intervention"),size = 1.8 )+
    geom_line(data = ebolaoutglo, aes(x=Time,y=Prev, colour ="No Intervention"),size = 1.8)+
    scale_color_manual(name = "Scenarios:", 
                       values = c("Intervention"="blue","No Intervention"="red"))+
    theme(legend.position="top")+
    ylim(0,max(max(ebolaoutglo$Prev),max(ebolaoutglonew$Prevnew)))+
    labs(x = "Days", y = "Number of Infected Individuals")+
    ggtitle("Global Prevalence")+
    theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      plot.title = element_text(size= 20, face = "bold"),
      plot.background = element_rect(fill="aliceblue", color = NA),
      panel.background = element_rect(fill = "white",colour = NA),
      panel.grid.major  = element_line(color = "gray91"))
  
  
  p2 = ggplot()+
    geom_line(data= ebolaoutglonew, aes(x=Timenew,Cumnew,colour ="Intervention"),size = 1.8 )+
    geom_line(data= ebolaoutglo, aes(x=Time,Cum,colour ="No Intervention"),size = 1.8 )+
    scale_color_manual(name = "", 
                       values = c("Intervention"="blue","No Intervention"="red"))+
    theme(legend.position="top")+
     ylim(0,max(max(ebolaoutglonew$Cumnew),max(ebolaoutglo$Cum)))+
    labs(x = "Days", y="")+
    ggtitle("Global Cumulative Incidence")+
    theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      plot.title = element_text(size= 20, face = "bold"),
      plot.background = element_rect(fill="aliceblue", color = NA),
      panel.background = element_rect(fill = "white",colour = NA),
      panel.grid.major  = element_line(color = "gray91"))
  grid.arrange(p1,p2, nrow =1)
  # g2 <- cowplot::ggdraw(g) + 
  #   theme(plot.background = element_rect(fill="wheat1", color = NA))
  # plot(g2) # Not needed
})
####EbolaSourceIntervention
output$ebolasourcDisGraphInterv <- renderPlot({
  ebolaSourceCount <- c(0) #initialization
  ebolaSourceCount[1] <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaSouCoun]
 
  theebolaSour <-input$ebolaSouCoun
  nmea =length(dataebola()$Dyna[,1])
  ebolaoutsource = data.frame(matrix(nrow = nmea, ncol = 3))
  names(ebolaoutsource) = c("Time","Prev","Cum")
  
  ebolaoutsource$Time =  dataebola()$Dyna[,1]
  #print( seiroutwatch)
  ebolaoutsource$Prev =  dataebola()$Dyna[,(200*3+1+ebolaSourceCount)]
  ebolaoutsource$Cum =  dataebola()$Inci[,ebolaSourceCount]
  
  # New output due to intervention
  nmea =length(dataebolaInterv()$Dyna[,1])
  ebolaoutsourcenew = data.frame(matrix(nrow = nmea, ncol = 3))
  names(ebolaoutsourcenew) = c("Timenew","Prevnew","Cumnew")
  ebolaoutsourcenew$Timenew =  dataebolaInterv()$Dyna[,1]
  ebolaoutsourcenew$Prevnew =  dataebolaInterv()$Dyna[,(200*3+1+ebolaSourceCount)]
  ebolaoutsourcenew$Cumnew =  dataebolaInterv()$Inci[,ebolaSourceCount]
 
  p1 = ggplot()+
    geom_line(data=ebolaoutsourcenew, aes(x=Timenew,y=Prevnew,colour ="Intervention"),size = 1.8 )+
    geom_line(data = ebolaoutsource, aes(x=Time,y=Prev, colour ="No Intervention"),size = 1.8)+
    scale_color_manual(name = "Scenarios:", 
                       values = c("Intervention"="blue","No Intervention"="red"))+
    theme(legend.position="top")+
    ylim(0,max(max(ebolaoutsource$Prev),max(ebolaoutsourcenew$Prevnew)))+
    labs(x = "Days", y = "Number of Infected Individuals")+
    ggtitle("Prevalence")+
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size= 20, face = "bold"),
          plot.background = element_rect(fill="aliceblue", color = NA),
          panel.background = element_rect(fill = "white",colour = NA),
          panel.grid.major  = element_line(color = "gray91"))
  
  
  p2 = ggplot()+
    geom_line(data= ebolaoutsourcenew, aes(x=Timenew,Cumnew,colour ="Intervention"),size = 1.8 )+
    geom_line(data= ebolaoutsource, aes(x=Time,Cum,colour ="No Intervention"),size = 1.8 )+
    scale_color_manual(name = "", 
                       values = c("Intervention"="blue","No Intervention"="red"))+
    theme(legend.position="top")+
    ylim(0,max(max(ebolaoutsourcenew$Cumnew),max(ebolaoutsource$Cum)))+
    labs(x = "Days", y="")+
    ggtitle("Cumulative Incidence")+
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size= 20, face = "bold"),
          plot.background = element_rect(fill="aliceblue", color = NA),
          panel.background = element_rect(fill = "white",colour = NA),
          panel.grid.major  = element_line(color = "gray91"))
  grid.arrange(p1,p2, nrow =1)
})

####EbolaWatchIntervention
output$ebolawatDisGraphInterv <- renderPlot({
  ebolaWatchCount <- c(0)
  ebolaWatchCount[1] <- dataCouInfo1$Index[dataCouInfo1$Country == input$ebolaWatCoun]
  
  #print(dim(dataebola$Dyna))
  
  theebolaSour <-input$ebolaSouCoun
  #print(max(rowSums(datasir$Inci)))
  #print(length(datasir$Dyna[,1]))
  #print(dim(datasir$Inci))
  nmea =length(dataebola()$Dyna[,1])
  ebolaoutwatch = data.frame(matrix(nrow = nmea, ncol = 3))
  
  names(ebolaoutwatch) = c("Time","Prev","Cum")
  
  ebolaoutwatch$Time =  dataebola()$Dyna[,1]
  #print( seiroutwatch)
  ebolaoutwatch$Prev = dataebola()$Dyna[,(200*3+1+ebolaWatchCount)]
  ebolaoutwatch$Cum = dataebola()$Inci[,ebolaWatchCount]
  
  # New output due to intervention
  nmea =length(dataebolaInterv()$Dyna[,1])
  ebolaoutwatchnew = data.frame(matrix(nrow = nmea, ncol = 3))
  names(ebolaoutwatchnew) = c("Timenew","Prevnew","Cumnew")
  ebolaoutwatchnew$Timenew =  dataebolaInterv()$Dyna[,1]
  ebolaoutwatchnew$Prevnew = dataebolaInterv()$Dyna[,(200*3+1+ebolaWatchCount)]
  ebolaoutwatchnew$Cumnew = dataebolaInterv()$Inci[,ebolaWatchCount]

  p1 = ggplot()+
    geom_line(data=ebolaoutwatchnew, aes(x=Timenew,y=Prevnew,colour ="Intervention"),size = 1.8 )+
    geom_line(data = ebolaoutwatch, aes(x=Time,y=Prev, colour ="No Intervention"),size = 1.8)+
    scale_color_manual(name = "Scenarios:", 
                       values = c("Intervention"="blue","No Intervention"="red"))+
    theme(legend.position="top")+
    ylim(0,max(max(ebolaoutwatch$Prev),max(ebolaoutwatchnew$Prevnew)))+
    labs(x = "Days", y = "Number of Infected Individuals")+
    ggtitle("Prevalence")+
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size= 20, face = "bold"),
          plot.background = element_rect(fill="aliceblue", color = NA),
          panel.background = element_rect(fill = "white",colour = NA),
          panel.grid.major  = element_line(color = "gray91"))
  
  
  p2 = ggplot()+
    geom_line(data= ebolaoutwatchnew, aes(x=Timenew,Cumnew,colour ="Intervention"),size = 1.8 )+
    geom_line(data= ebolaoutwatch, aes(x=Time,Cum,colour ="No Intervention"),size = 1.8 )+
    scale_color_manual(name = "", 
                       values = c("Intervention"="blue","No Intervention"="red"))+
    theme(legend.position="top")+
    ylim(0,max(max(ebolaoutwatchnew$Cumnew),max(ebolaoutwatch$Cum)))+
    labs(x = "Days", y="")+
    ggtitle("Cumulative Incidence")+
    theme(axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          plot.title = element_text(size= 20, face = "bold"),
          plot.background = element_rect(fill="aliceblue", color = NA),
          panel.background = element_rect(fill = "white",colour = NA),
          panel.grid.major  = element_line(color = "gray91"))
  grid.arrange(p1,p2, nrow =1)
})
})