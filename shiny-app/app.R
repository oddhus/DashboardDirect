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
library(survminer)
library(survival)
library(ggpubr)
source("shiny-app/Utils.R")
source("shiny-app/FetchData.R")
source("shiny-app/ImplantModule.R")
source("shiny-app/ExplorerPlotModule.R")
source("shiny-app/ClinicInfoBoxes.R")
source("shiny-app/AnalyzePlotModule.R")
source("shiny-app/ReportModule.R")
source("shiny-app/StdReportModule.R")
source("shiny-app/SurvivalModule.R")
source("shiny-app/ImplantSurvivalModule.R")
source("shiny-app/ClinicModule.R")

ui <- dashboardPage(
  dashboardHeader(title = "Tooth implants"),
  dashboardSidebar(
    sidebarMenu(

        tags$style(type = "text/css",
                   "#sidebarTitle { padding-left: 16px; } "),
        div(id = "sidebarTitle", h3("Graph options")),
      conditionalPanel(
        condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Insertions'",
        analyzePlotInputUI("InsertionsAnalyze")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Removals'",
        analyzePlotInputUI("RemovalsAnalyze")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Overview Removals'",
        implantInputUI("RemovalReason")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Survival analysis'",
        survivalPlotInputUI("SurvivalPlot")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Clinic'",
        clinicInputUI("ClinicInfo")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Implant Survival'",
        implantSurvivalPlotInputUI("ImplantSurvival")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer'",
        explorePlotOptionsUI("Explorer")
      )
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    tabsetPanel(
      id = "tabs",
      tabPanel(
        "Overview Removals",
        fluidRow(
          column(
            implantPlotUI("RemovalReason"),
            width = 12,
            style='margin-top:10px;'
          )
        )
        
      ),
      tabPanel(
        "Survival analysis",
        fluidRow(
          box(
            title = "Implant Survival",
            survivalPlotUI("SurvivalPlot"),
            width = 12
          ),
          style='margin-top:10px;'
        ),
        
      ),
      tabPanel(
        "Implant Survival",
        fluidRow(
          box(
            title = "Implant Survival",
            implantSurvivalPlotUI("ImplantSurvival"),
            width = 6
          ),
          column(
            box(
              title = "Drilldown options",
              implantLotNrSurvivalPlotInputUI("ImplantSurvival"),
              width = 12
            ),
            box(
              title = "Drilldown LotNr Survival",
              implantLotNrSurvivalPlotUI("ImplantSurvival"),
              width = 12
            ),
            width = 6
          ),
          
          style='margin-top:10px;'
        ),
        
      ),
      tabPanel(
        "Clinic",
        fluidRow(
          box(
            title = "Operations statistics",
            fluidRow(            
              clinicAntibioticInfoUI("ClinicInfo"),
              complicationsInfoUI("ClinicInfo"),
              guiderailInfoUI("ClinicInfo"),
              width = 12
            ),
            width = 3,
            height = 400
          ),
          box(
            title = "The 20 most recent operated implants",
            clincToothImplantPlotUI("ClinicInfo"),
            width = 3
          ),
          box(
            title = "Implant survival by clinic",
            column(clinicSuccessRatePlotUI("ClinicInfo"), width = 12),
            width = 6
          ),
          style='margin-top:10px;'
        ),
        fluidRow(
          box(
            title = "Change in statistics over time",
            tabsetPanel(
              tabPanel(
                "Antibiotics Usage",
                column(clinicAntibioticUsagePlotUI("ClinicInfo"), width = 12)
              ),
              tabPanel(
                "First Year Infections",
                column(clinicInfectionPlotUI("ClinicInfo"), width = 12)
              )
            ),
            width = 12
          )
        )
      ),
      tabPanel(
        "Explorer",
        fluidRow(
          column(
            explorerPlotUI("Explorer"),
            width = 12
          ),
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
  # ------------------------------s---------------------------------------------
  completeTable <- getCompleteTable()
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

  implantServer("RemovalReason", completeTable %>% filter(!is.na(RemovalId)), plotsInReport)

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

  explorerPlotServer("Explorer", data = completeTable, plotInReport = plotsInReport )

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
  # Survival
  #----------------------------------------------------------------------------
  survivalPlotServer("SurvivalPlot", data = completeTable, plotInReport = plotsInReport)
  
  implantSurvivalServer("ImplantSurvival", data = completeTable, plotInReport = plotsInReport)
  
  #----------------------------------------------------------------------------
  # Clinic
  #----------------------------------------------------------------------------
  clinicServer("ClinicInfo", data = completeTable, plotInReport = plotsInReport)
}

shinyApp(ui, server)

