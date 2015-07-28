# Sketches for devdp course projectr shiny app
# Data preprocess

# download.file("http://archive.wired.com/wired/archive/17.02/1702_Infoporn_Playmate_Data.xls", 
#               "initial_data.xls", method = "wget")
# library(gdata)
# df0 <- read.xls("./Initial_data.xls")

library(dplyr)
library(birk) # Contains "conv_unit" function for unit conversion

d.all <- read.csv("projectdata.csv", 
                  col.names = c("Month", "Year", 
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
# Filter required year range

year.range <- c("1990", "2009")
SI.units   <- FALSE

source("plotEveryBody.R")
source("plotYearlyAverages.R")

fdfile <- paste(paste(year.range[1], year.range[2], sep = "-"), "RData", sep = ".")

d.all.filtered <- d.all %>% 
        filter(Year >= as.integer(year.range[1]) & Year <= as.integer(year.range[2]))
d.year.filtered <- d.year %>%
        filter(Year >= as.integer(year.range[1]) & Year <= as.integer(year.range[2]))

save(year.range, SI.units, d.all.filtered, d.year.filtered, file = fdfile, compress = TRUE)