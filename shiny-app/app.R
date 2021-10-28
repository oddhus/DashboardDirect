library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(httr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(patchwork)
library(shinyWidgets)
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
      menuItem("Clinic", tabName = "clinic", icon = icon("th")),
      menuItem("Explorer", tabName = "explorer", icon = icon("plane")),
      menuItem("Models", tabName = "models", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      # Second tab content
      tabItem(
        tabName = "clinic",
        fluidRow(
          column(
            h3("Information"),
            htmlOutput("selectClinicName"),
            width = 6
          ),
          column(
            valueBoxOutput("complicationBox", width = 6),
            valueBoxOutput("insertionsBox", width = 6),
            width = 6
          )
        ),
        fluidRow(
          column(
            plotOutput("clinicComparePlot", height = 500) %>% withSpinner(),
            width = 12
          )
        ),
        fluidRow(column(12, h4("Options"))),
        fluidRow(
          column(
            htmlOutput("selectYAxisClinic", width = 12),
            htmlOutput("selectClinicCompare", width = 12),
            width = 3
          ),
          column(
            htmlOutput("selectXAxisClinic", width = 12),
            htmlOutput("selectCompareAttribute", width = 12),
            width = 3
          ),
          column(
            htmlOutput("selectClinicFacetRow", width = 12),
            htmlOutput("selectSpecificFacetRow", width = 12),
            width = 3
          ),
          column(
            materialSwitch(inputId = "showMean", label = "Show mean values"),
            materialSwitch(inputId = "hideXLab", label = "Hide X-axis labels"),
            width = 3
          ),
        )

        # fluidRow(
        #   box(plotOutput("complicationsPlot", height = 300) %>% withSpinner(), width = 12)
        # ),
        # fluidRow(
        #   box(
        #     plotOutput("clinicOverTime",
        #       height = 400,
        #       dblclick = "clinicOverTime_click",
        #       brush = brushOpts(
        #         id = "clinicOverTime_brush",
        #         resetOnNew = TRUE
        #       )
        #     ) %>% withSpinner(),
        #     width = 12
        #   )
        # ),
        # fluidRow(
        #   box(
        #     selectInput("variable", "Variable:",
        #       c(
        #         "Insertions" = "insertions",
        #         "Complications" = "complications",
        #         "Antibiotics Pre Operation" = "antibioticsUsageBefore"
        #       ),
        #       multiple = TRUE
        #     ),
        #   )
        # )
      ),
      tabItem(
        tabName = "explorer",
        h2("Data explorer"),
        plotOutput("plot2", height = "500px") %>% withSpinner(),
        hr(),
        fluidRow(
          column(
            3,
            htmlOutput("selectYAxis"),
          ),
          column(
            4,
            htmlOutput("selectFactor"),
            htmlOutput("selectImplants"),
            htmlOutput("selectColor"),
          ),
          column(
            4,
            htmlOutput("selectFacetRow"),
            htmlOutput("selectFacetCol"),
          ),
        ),
      ),
      tabItem(
        tabName = "models",
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
            width = 2
          ),
          box(
            title = "Implant models' complication percentage",
            status = "primary",
            plotOutput("LotNrComplication") %>% withSpinner(),
            width = 8,
          ),
          box(
            title = "Fisher Test",
            status = "info",
            textOutput("LotNrFisherTest") %>% tagAppendAttributes(style = "white-space:pre-wrap;"),
            width = 2
          )
        ),
      )
    )
  )
)

server <- function(input, output, session) {
  insertions <- getInsertions()
  implants <- getImplants()
  insertionsWithImplants <- implants %>%
    left_join(insertions, by = c("InsertionId" = "Id")) %>%
    rename(InsertionDate = InsertionDate.y)

  # Tab 1
  ## Length and diamter plot
  output$implantComplications <- renderPlot({
    implantComplicationPlot(insertionsWithImplants, input$highlightPercentage)
  })

  output$implantDiamterLength <- renderPlot({
    implantLengthDiameterPlot(insertionsWithImplants, input$PositionSelect, "Length") +
      implantLengthDiameterPlot(insertionsWithImplants, input$PositionSelect, "Diameter")
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

  output$LotNrFisherTest <- renderText({
    lotNrFisherTest(insertionsWithImplants, input$LotNrImplantSelect)
  })

  # Tab 2
  ranges <- reactiveValues(x = NULL)

  output$selectClinicName <- renderUI({
    pickerInput("selectClinic",
      "Select Clinic",
      choices  = unique(insertions$Clinic)
    )
  })

  output$selectClinicCompare <- renderUI({
    pickerInput("selectClinicCompare",
      "Compare Clinics",
      # Do not compare against the current selected clinic
      choices = unique(insertions$Clinic[insertions$Clinic != input$selectClinic]),
      multiple = T,
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      )
    )
  })

  output$selectClinicFacetRow <- renderUI({
    selectClinicFacetRowControl(insertionsWithImplants)
  })

  output$selectSpecificFacetRow <- renderUI({
    if (input$selectClinicFacetRowControl == "None") {
      HTML("<div></div>")
    } else {
      selectSpecificFacetRowControl(insertionsWithImplants, input$selectClinicFacetRowControl)
    }
  })

  output$selectCompareAttribute <- renderUI({
    if (input$selectXAxisClinicControl == "Clinic") {
      HTML("<div></div>")
    } else {
      selectCompareAttributeControl(insertionsWithImplants, input$selectXAxisClinicControl)
    }
  })

  output$selectYAxisClinic <- renderUI({
    selectYAxisClinicControl(insertionsWithImplants)
  })

  output$selectXAxisClinic <- renderUI({
    selectXAxisClinicControl(insertionsWithImplants)
  })

  output$clinicComparePlot <- renderPlot({
    clinicComparePlot(
      insertionsWithImplants,
      input$showMean,
      input$hideXLab,
      input$selectClinic,
      input$selectClinicCompare,
      input$selectClinicFacetRowControl,
      input$selectSpecificFacetRowControl,
      input$selectCompareAttributeControl,
      input$selectYAxisClinicControl,
      input$selectXAxisClinicControl
    )
  })

  output$complicationBox <- renderValueBox({
    complicationsInfo(insertionsWithImplants, input$selectClinic)
  })

  output$insertionsBox <- renderValueBox({
    insertionsInfo(insertionsWithImplants, input$selectClinic)
  })



  # output$clinicOverTime <- renderPlot({
  #   clinicPlot(insertionsWithImplants, input$selectClinicTab2, input$variable, ranges$x)
  # })

  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.

  # observeEvent(input$clinicOverTime_click, {
  #   brush <- input$clinicOverTime_brush
  #   if (!is.null(brush)) {
  #     ranges$x <- c(
  #       as.Date(brush$xmin, origin = "1970-01-01"),
  #       as.Date(brush$xmax, origin = "1970-01-01")
  #     )
  #   } else {
  #     ranges$x <- NULL
  #   }
  # })
  #
  # output$complicationsPlot <- renderPlot({
  #   complicationsPlot(insertionsWithImplants, input$selectClinicTab2)
  # })


  # Tab 3 - Data explorer
  ## Factor plot
  output$selectImplants <- renderUI({
    selectInsertionAttributeControl(insertionsWithImplants, input$selectFactorControl)
  })

  output$selectFactor <- renderUI({
    selectFactorControl(insertionsWithImplants)
  })

  output$selectColor <- renderUI({
    selectColorControl(insertionsWithImplants)
  })

  output$selectFacetRow <- renderUI({
    selectFacetRowControl(insertionsWithImplants)
  })

  output$selectFacetCol <- renderUI({
    selectFacetColControl(insertionsWithImplants)
  })

  output$selectYAxis <- renderUI({
    selectYAxisControl(insertionsWithImplants)
  })

  output$plot2 <- renderPlot({
    factorPlot(
      insertionsWithImplants,
      input$selectYAxisControl,
      input$selectFactorControl,
      input$selectInsertionAttributeControl,
      input$selectColorControl,
      input$selectFacetRowControl,
      input$selectFacetColControl
    )
  })
}

shinyApp(ui, server)
