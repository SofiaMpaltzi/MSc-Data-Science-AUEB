# Libraries
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

Sys.setlocale("LC_TIME", "C")

# Read and transform data
data = read_excel("[path of data file]COVID-19-geographic-disbtribution-worldwide.xlsx")

data = data %>% 
  dplyr::mutate(cases = dplyr::case_when(cases<0 ~ 0,
                                         TRUE ~ cases),
                month = months(dateRep)) %>% 
  dplyr::group_by(month, continentExp) %>% 
  dplyr::summarise(cases = sum(cases),
                   deaths = sum(deaths)) %>% 
  data.frame()

# Barchart 1
data1 = 
  tidyr::spread(data=data, key=continentExp, value=cases) %>% 
  dplyr::mutate(All = Africa + America + Asia + Europe + Oceania) %>% 
  dplyr::select(-Other) %>% 
  data.frame()

# Barchart 1
data2 = 
  tidyr::spread(data=data, key=continentExp, value=deaths) %>% 
  dplyr::mutate(All = Africa + America + Asia + Europe + Oceania) %>% 
  dplyr::select(-Other) %>% 
  data.frame()

# Create server  
shinyServer(function(input, output) {
  
  # Filter of continent
  datasetInput1 <- reactive({
    
    data1 = data1 %>% 
      dplyr::mutate(selected_continent = input$userInput)
    data1
    
  })
  
  datasetInput2 <- reactive({
    
    data2 = data2 %>% 
      dplyr::mutate(selected_continent = input$userInput)
    data2
    
  })

 
  # Bar plot
  output$barchart1 <- renderPlot({
    
    datasetInput1 <- datasetInput1()
    
    p = ggplot(datasetInput1, aes(x=month, y = datasetInput1[[as.name(selected_continent)]])) +
      geom_bar(stat = 'identity', 
               position = "dodge", 
               fill = "Sky Blue 4") +
      geom_text(label = format(datasetInput1[[as.name(input$userInput)]], big.mark = ",", scientific = FALSE), 
                size = 3, 
                vjust = -0.4) +
      scale_fill_distiller(palette = "Spectral") +
      xlab("Month") + 
      ylab("Cases") +
      xlim("December","January","February","March","April","May","June","July","August","September","October","November") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))

    p
  
    })
  
  output$barchart2 <- renderPlot({
    
    datasetInput2 <- datasetInput2()
    
    p = ggplot(datasetInput2, aes(x=month, y = datasetInput2[[as.name(selected_continent)]])) +
      geom_bar(stat = 'identity', 
               position = "dodge", 
               fill = "firebrick3") +
      geom_text(label = format(datasetInput2[[as.name(input$userInput)]], big.mark = ",", scientific = FALSE), 
                size = 3, 
                vjust = -0.4) +
      scale_fill_distiller(palette = "Spectral") +
      xlab("Month") + 
      ylab("Deaths") +
      xlim("December","January","February","March","April","May","June","July","August","September","October","November") +
      scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
    
    p
    
  })

  })