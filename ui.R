library(shiny)
library(rCharts)
finalData <- readRDS("data/finalData.rds")

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
        headerPanel("Health and Economic Impact of Storms in the U.S."),
        sidebarPanel(
                selectInput("type", 'Select Event Type', unique(levels(finalData$adjType))),
                selectInput("column", 'Select Impact', c("Fatalities","Injuries","Property.Damage","Crop.Damage")),
                numericInput("ncuts", 'Select Number of Bins', value = 50, min = 10, max = 50),
                sliderInput("year", "Year:", min=1995, max=2011, value=2011, step = 1, sep = "")
        ),
        mainPanel(
                rCharts::chartOutput('myplot', 'datamaps'),
                tags$div("This map shows either the number of Injuries/Fatalities or the Cost of Property/Crop Damage associated with each weather event type.",
                         tags$br(),
                         "The event type, impact type, and year can be changed using the selection boxes and slider to the left.",
                         tags$br(),
                         "The data can be allocated in up to 50 bins. Under some conditions more bins can make subtle differences between states more obvious.")
        )
))