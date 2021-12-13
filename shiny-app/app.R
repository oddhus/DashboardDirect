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
source("shiny-app/Utils.R")
source("shiny-app/FetchData.R")
source("shiny-app/ImplantModule.R")
source("shiny-app/ExplorerPlotModule.R")
source("shiny-app/ClinicInfoBoxes.R")
source("shiny-app/AnalyzePlotModule.R")
source("shiny-app/ClinicSelectModule.R")
source("shiny-app/ReportModule.R")
source("shiny-app/StdReportModule.R")
source("shiny-app/TimeSeriesModule.R")

ui <- dashboardPage(
  dashboardHeader(title = "Tooth implants"),
  dashboardSidebar(
    sidebarMenu(

        tags$style(type = "text/css",
                   "#sidebarTitle { padding-left: 16px; } "),
        div(id = "sidebarTitle", h3("Options")),

      conditionalPanel(
        condition = "input.tabs == 'Explorer' | input.tabs == 'Analyses'",
        radioGroupButtons(
          inputId = "insertionsOrRemovals",
          label = "Type",
          choices = c("Insertions", "Removals"),
          status = "primary"
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer'",
        radioGroupButtons(
          inputId = "clinicOrAllcombined",
          label = "Grouping",
          choices = c("Clinic", "All combined"),
          status = "primary"
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer' & input.clinicOrAllcombined == 'Clinic'",
        clinicSelectUI("Clinic")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer' & input.insertionsOrRemovals == 'Insertions'",
        explorePlotOptionsUI("Insertions"),
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer' & input.insertionsOrRemovals == 'Removals'",
        explorePlotOptionsUI("Removals")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Insertions'",
        analyzePlotInputUI("InsertionsAnalyze")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Removals'",
        analyzePlotInputUI("RemovalsAnalyze")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Implants'",
        implantInputUI("RemovalReason")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Time series'",
        timeSeriesInputUI("Time")
      )
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Implants",
        h4("Overview of removed Implants"),
        column(
          implantPlotUI("RemovalReason"),
          width = 12
        )
      ),
      tabPanel(
        "Time series",
        h4("Implants over time"),
        timeSeriesPlotUI("Time")
      ),
      tabPanel(
        "Explorer",
        fluidRow(
          column(
            explorerPlot("Insertions"),
            explorerPlot("Removals"),
            width = 12
          ),
        ),
        fluidRow(
          column(
            infoBoxesUI("InfoBoxes"),
            width = 12
          )
        ),
      ),
      tabPanel(
        "Analyses",
        h4("Analyze data"),
        fluidRow(
          column(
            analyzePlotUI("InsertionsAnalyze"),
            analyzePlotUI("RemovalsAnalyze"),
            width = 6
          ),
          column(
            analyzePrintUI("InsertionsAnalyze"),
            analyzePrintUI("RemovalsAnalyze"),
            width = 6
          )
        )
      ),
      tabPanel(
        "Report",
        h4("Generate a report"),
        reportUI("Report")
      ),
      tabPanel(
        "Standard Report",
        h4("Standard tooth implant registry report"),
        stdReportUI("StdReport")
      )
    )
  )
)

server <- function(input, output, session) {
  columnsToRemove <- c("RefNr", "Id.y", "Id", "ComplicationsComment", "AntibioticsType")
  facetRowsColumnsToRemove <- c("LotNr")

  # ---------------------------------------------------------------------------
  # Data
  # ---------------------------------------------------------------------------

  insertionsWithImplants <- getInsertionsWithImplants()
  removalsWithImplants <- getRemovalsWithImplants()

  # ---------------------------------------------------------------------------
  # Global events and variables
  # ---------------------------------------------------------------------------

  showInsertions <- reactiveVal(TRUE)
  showRemovals <- reactiveVal(FALSE)
  plotsInReport <- reactiveValues()
  

  # Hides the plot not currently viewed by user, avoiding to generate the hidden
  # plot again
  observeEvent(input$insertionsOrRemovals, {
    if (input$insertionsOrRemovals == "Insertions") {
      showInsertions(TRUE)
      showRemovals(FALSE)
    } else if (input$insertionsOrRemovals == "Removals") {
      showInsertions(FALSE)
      showRemovals(TRUE)
    }
  })

  # ---------------------------------------------------------------------------
  # Removals Plot
  # ---------------------------------------------------------------------------

  implantServer("RemovalReason", removalsWithImplants, plotsInReport)

  #----------------------------------------------------------------------------
  # Explorer plot
  #----------------------------------------------------------------------------

  ## Reactive values ----------------------------------------------------------

  allCombined <- reactiveVal(FALSE)

  ## Observe events -----------------------------------------------------------

  ### Update relevant values when switching from clinic to all combined mode
  observeEvent(input$clinicOrAllcombined, {
    if ("Clinic" %in% input$clinicOrAllcombined) {
      allCombined(FALSE)
    } else {
      allCombined(TRUE)
    }
  })

  ## Server logic ------------------------------------------------------------

  clinicValues <- clinicSelectServer("Clinic", insertionsWithImplants)

  explorerPlotServer("Insertions", insertionsWithImplants, columnsToRemove,
    allCombined,
    selectedClinic = clinicValues[[1]],
    clinicCompare = clinicValues[[2]],
    isVisible = showInsertions,
    plotInReport = plotsInReport
  )

  explorerPlotServer("Removals", removalsWithImplants, columnsToRemove,
    allCombined,
    selectedClinic = clinicValues[[1]],
    clinicCompare = clinicValues[[2]],
    isVisible = showRemovals,
    plotInReport = plotsInReport
  )

  # 3 Info-boxes --------------------------------------------------------------
  infoBoxesModule(
    "InfoBoxes",
    insertionsWithImplants,
    removalsWithImplants,
    allCombined,
    clinicValues[[1]]
  )

  #----------------------------------------------------------------------------
  # Analyses
  #----------------------------------------------------------------------------

  analyzePlotServer("InsertionsAnalyze",
    insertionsWithImplants,
    columnsToRemove,
    isVisible = showInsertions,
    plotInReport = plotsInReport
  )

  analyzePlotServer("RemovalsAnalyze",
    removalsWithImplants,
    columnsToRemove,
    isVisible = showRemovals,
    plotInReport = plotsInReport
  )

  #----------------------------------------------------------------------------
  # Report
  #----------------------------------------------------------------------------
  
  reportServer("Report", 
               insertionsWithImplants,
               removalsWithImplants,
               plotsInReport)
  
  stdReportServer("StdReport",
                  insertionsWithImplants,
                  removalsWithImplants)
  
  #----------------------------------------------------------------------------
  # TimeSeries
  #----------------------------------------------------------------------------
  
  timeSeriesServer("Time", data = removalsWithImplants, plotInReport = plotsInReport)
}

shinyApp(ui, server)

