library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(httr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(patchwork)
source("shiny-app/FetchData.R")
source("shiny-app/Utils.R")
source("shiny-app/AntibioticPlot.R")
source("shiny-app/FactorPlot.R")
source("shiny-app/ClinicInfo.R")
source("shiny-app/ImplantPlot.R")

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
            status = "info",
            selectInput("PositionSelect", choices = seq(1, 100, by = 1), label = "Select Position"),
            width = 3
          ),
          box(
            title = "Implant Characteristics Complications",
            status = "primary",
            plotOutput("implantDiamterLength") %>% withSpinner(),
            width = 9
          ),
        ),
        fluidRow(
          box(
            status = "info",
            title = "Controls",
            htmlOutput("selectFactor"),
            htmlOutput("selectImplants"),
            width = 3
          ),
          box(
            status = "primary",
            plotOutput("plot2", height = 300) %>% withSpinner(),
            width = 9
          ),
        ),
        fluidRow(
          box(
            status = "info",
            sliderInput(
              "highlightPercentage",
              label = "Show id of implant with complication percentage or higher",
              min = 15, max = 100, value = 50
            ),
            width = 3
          ),
          box(
            title = "",
            status = "primary",
            plotOutput("implantComplications") %>% withSpinner(),
            width = 9,
          )
        ),
        fluidRow(
          box(
            status = "info",
            title = "Select Implant",
            htmlOutput("selectImplantName"),
            width = 3
          ),
          box(
            title = "Implant models' complication percentage",
            status = "primary",
            plotOutput("LotNrComplication") %>% withSpinner(),
            width = 9,
          )
        ),
      ),
      # Second tab content
      tabItem(
        tabName = "clinic",
        h2("Clinic Information"),
        fluidRow(
          box(
            htmlOutput("selectClincNamesTab2"),
            width = 6
          ),
          valueBoxOutput("complicationBox", width = 3),
          valueBoxOutput("insertionsBox", width = 3),
        ),
        fluidRow(
          box(plotOutput("complicationsPlot", height = 300) %>% withSpinner(), width = 12)
        ),
        fluidRow(
          box(
            plotOutput("clinicOverTime",
              height = 400,
              dblclick = "clinicOverTime_click",
              brush = brushOpts(
                id = "clinicOverTime_brush",
                resetOnNew = TRUE
              )
            ) %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(
          box(
            selectInput("variable", "Variable:",
              c(
                "Insertions" = "insertions",
                "Complications" = "complications",
                "Antibiotics Pre Operation" = "antibioticsUsageBefore"
              ),
              multiple = TRUE
            ),
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  insertions <- getInsertions()
  implants <- getImplants()
  insertionsWithImplants <- implants %>% left_join(insertions, by = c("InsertionId" = "Id"))

  # Tab 1
  ## Length and diamter plot
  output$implantComplications <- renderPlot({
    implantComplicationPlot(insertionsWithImplants, input$highlightPercentage)
  })

  output$implantDiamterLength <- renderPlot({
    implantLengthDiameterPlot(insertionsWithImplants, input$PositionSelect, "Length") +
      implantLengthDiameterPlot(insertionsWithImplants, input$PositionSelect, "Diameter")
  })

  ## Factor plot
  output$selectImplants <- renderUI({
    selectInsertionAttributeControl(implants, input$selectFactorControl)
  })

  output$selectFactor <- renderUI({
    selectFactorControl(implants)
  })

  output$plot2 <- renderPlot({
    factorPlot(implants, input$selectFactorControl, input$selectInsertionAttributeControl)
  })


  ## LotNr Complications plot
  output$selectImplantName <- renderUI({
    selectInput("LotNrImplantSelect",
      "Select Implant",
      choices  = unique(implants$ImplantName),
      selected = "None",
      multiple = F,
    )
  })

  output$LotNrComplication <- renderPlot({
    lotNrComplications(insertionsWithImplants, input$LotNrImplantSelect)
  })

  # Tab 2
  ranges <- reactiveValues(x = NULL)

  output$selectClincNamesTab2 <- renderUI({
    selectInput("selectClinicTab2",
      "Select Clinic",
      choices  = unique(insertions$ClinicId),
      selected = 1
    )
  })

  output$complicationBox <- renderValueBox({
    complicationsInfo(insertionsWithImplants, input$selectClinicTab2)
  })

  output$insertionsBox <- renderValueBox({
    insertionsInfo(insertionsWithImplants, input$selectClinicTab2)
  })

  output$clinicOverTime <- renderPlot({
    clinicPlot(insertionsWithImplants, input$selectClinicTab2, input$variable, ranges$x)
  })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$clinicOverTime_click, {
    brush <- input$clinicOverTime_brush
    if (!is.null(brush)) {
      ranges$x <- c(as.Date(brush$xmin, origin = "1970-01-01"), as.Date(brush$xmax, origin = "1970-01-01"))
    } else {
      ranges$x <- NULL
    }
  })

  output$complicationsPlot <- renderPlot({
    complicationsPlot(insertionsWithImplants, input$selectClinicTab2)
  })
}

shinyApp(ui, server)
