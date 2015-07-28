library(markdown)

shinyUI(fluidPage(
        titlePanel("Biometric data view"),
        
        sidebarLayout(
               sidebarPanel(
                       radioButtons("SI.units", label = "System of measurements",
                                    choices = list("Metric system (SI)" = TRUE, 
                                                   "US customary units" = FALSE), 
                                    selected = TRUE),
                       hr(),
                       
                       sliderInput("year.range", label ="Year range", step = 1, sep = "",
                                   min = 1953, max = 2009, value = c(1970, 1990)),
                       hr()

               ),
               mainPanel(
                       tabsetPanel(type = "tabs",
                                   tabPanel("All data", plotOutput("plot1")),
                                   tabPanel("Averaged by year", plotOutput("plot2")),
                                   tabPanel("Help", includeMarkdown("helptext.md"))
                       )
               )
        )
))
