library(dplyr)
library(birk) # Contains "conv_unit" function for unit conversion

## Read and preprocess data
d.all <- read.csv("projectdata.csv", col.names = c("Month", "Year", 
                                       "BustSI", "WaistSI", "HipsSI",
                                       "HeightSI", "WeightSI")) %>%
        
        # Introducing new variable(s)
        mutate(Period = as.Date(paste("01", as.character(Month), as.character(Year)), 
                                format = "%d %B %Y"),
               BMI = WeightSI / (HeightSI / 100) ^ 2,
               UnderWeight = ifelse (BMI < 18.5, TRUE, FALSE)) %>%
        
        # Adding variables in US customary units
        mutate(BustUS   = round(conv_unit(BustSI,   "cm", "inch")),
               WaistUS  = round(conv_unit(WaistSI,  "cm", "inch")),
               HipsUS   = round(conv_unit(HipsSI,   "cm", "inch")),
               HeightUS = round(conv_unit(HeightSI, "cm", "inch") * 2) / 2,
               WeightUS = round(conv_unit(WeightSI, "kg", "lbs")))

d.year <- d.all %>% group_by(Year) %>% summarize(BustSI   = mean(BustSI),
                                                 WaistSI  = mean(WaistSI),
                                                 HipsSI   = mean(HipsSI),
                                                 WeightSI = mean(WeightSI), 
                                                 HeightSI = mean(HeightSI),
                                                 BustUS   = mean(BustUS),
                                                 WaistUS  = mean(WaistUS),
                                                 HipsUS   = mean(HipsUS),
                                                 WeightUS = mean(WeightUS), 
                                                 HeightUS = mean(HeightUS),
                                                 BMI      = mean(BMI))

# Functions (called by shinyServer) definition
source("plotEveryBody.R")
source("plotYearlyAverages.R")

## Shiny part

library(shiny)
shinyServer(function(input, output) {
        
        output$SI.units   <- renderPrint({ input$SI.units })
        output$year.range <- renderPrint({ input$year.range })
        
        output$plot1 <- renderPlot({
                
                d.all.filtered <- d.all %>% 
                        filter(Year >= as.integer(input$year.range[1]) & 
                               Year <= as.integer(input$year.range[2]))
                plotEveryBody(d.all.filtered, input$SI.units)
        })
        output$plot2 <- renderPlot({
                
                d.year.filtered <- d.year %>% 
                        filter(Year >= as.integer(input$year.range[1]) & 
                               Year <= as.integer(input$year.range[2]))
                plotYearlyAverages(d.year.filtered, input$SI.units)
        })
})