library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(httr)
library(ggplot2)
library(dplyr)
source("shiny-app/FetchData.R")
source("shiny-app/Utils.R")
source("shiny-app/AntibioticPlot.R")
source("shiny-app/FactorPlot.R")

ui <- dashboardPage(
  dashboardHeader(title = "Tooth implants"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Clinic", tabName = "clinic", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        # First tab content
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(
            plotOutput("plot1", height = 250) %>% withSpinner()
          ),
          box(
            title = "Controls",
            htmlOutput("selectClincNames")
          )
        ),
        fluidRow(
          box(plotOutput("plot2", height = 300) %>% withSpinner()),
          box(
            title = "Controls",
            htmlOutput("selectFactor"),
            htmlOutput("selectImplants")
          )
        )
      ),
      # Second tab content
      tabItem(
        tabName = "clinic",
        h2("Clinic Information"),
        box(
          htmlOutput("selectClincNamesTab2")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  insertions <- getInsertions()
  implants <- getImplants()

  output$selectClincNames <- renderUI({
    selectInput("select1",
      "Select Clinic",
      choices  = unique(insertions$ClinicId),
      multiple = TRUE
    )
  })
  
  output$selectClincNamesTab2 <- renderUI({
    selectInput("selectClinicTab2",
                "Select Clinic",
                choices  = unique(insertions$ClinicId),
    )
  })
  

  output$selectFactor <- renderUI({
    selectFactorControl(implants)
  })

  output$selectImplants <- renderUI({
    selectInsertionAttributeControl(implants, input$selectFactorControl)
  })

  output$plot1 <- renderPlot({
    antibioticPlot(insertions, input$select1)
  })

  output$plot2 <- renderPlot({
    factorPlot(implants, input$selectFactorControl, input$selectInsertionAttributeControl)
  })
}

shinyApp(ui, server)
