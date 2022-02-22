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
        condition = "input.tabs == 'Analyses'",
        analyzePlotInputUI("Analyze")
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
          box(
            title = "Removal reasons over time",
            implantPlotUI("RemovalReason"),
            width = 12,
          ),
          style='margin-top:10px;'
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
          style='margin-top:10px;'
        ),
      ),
      tabPanel(
        "Analyses",
        fluidRow(
          column(
            analyzePlotUI("Analyze"),
            width = 6
          ),
          column(
            analyzePrintUI("Analyze"),
            width = 6
          ),
          style='margin-top:10px;'
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

  # ---------------------------------------------------------------------------
  # Data
  # ------------------------------s---------------------------------------------
  completeTable <- getCompleteTable()

  # ---------------------------------------------------------------------------
  # Global events and variables
  # ---------------------------------------------------------------------------

  plotsInReport <- reactiveValues()
  

  # ---------------------------------------------------------------------------
  # Removals Plot
  # ---------------------------------------------------------------------------

  implantServer("RemovalReason", data, plotsInReport)

  #----------------------------------------------------------------------------
  # Explorer plot
  #----------------------------------------------------------------------------

  explorerPlotServer("Explorer", data = completeTable, plotInReport = plotsInReport )

  #----------------------------------------------------------------------------
  # Analyses
  #----------------------------------------------------------------------------

  analyzePlotServer("Analyze", completeTable, plotInReport = plotsInReport)

  #----------------------------------------------------------------------------
  # Report
  #----------------------------------------------------------------------------
  
  reportServer("Report", data, plotsInReport)
  
  stdReportServer("StdReport",data)
  

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

