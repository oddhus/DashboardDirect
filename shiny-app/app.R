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
source("shiny-app/FactorSurvivalModule.R")
source("shiny-app/ClinicModule.R")

ui <- dashboardPage(
  dashboardHeader(title = "Tooth implants"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      tags$style(
        type = "text/css",
        "#sidebarTitle { padding-left: 16px; } "
      ),
      div(id = "sidebarTitle", h3("Options")),
      conditionalPanel(
        condition = "input.tabs == 'Overview Removals' |
                      input.tabs == 'Survival analysis' |
                      input.tabs == 'Survival Factors' |
                      input.tabs == 'Clinic' |
                      input.tabs == 'Explorer'",
        sidebarMenu(menuItem("Global Filter",
          tabName = "filter",
          pickerInput("overallFilter",
            "Select Global Filter",
            choices = c(
              "LekholmZarbVolume",
              "LekholmZarbDensity",
              "TimeUntilLoad",
              "Stability",
              "BoneAugmentationMethod",
              "None"
            ),
            selected = "None"
          ),
          htmlOutput("overallFilterLevels"),
          style = "padding-bottom:6px;"
        ))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Overview Removals'",
        sidebarMenu(menuItem("Basic options",
          tabName = "basic",
          implantInputBasicUI("RemovalReason"),
          startExpanded = T,
          style = "padding-bottom:6px;"
        ))
      ),
      conditionalPanel(
        condition = "input.tabs == 'Overview Removals'",
        sidebarMenu(
          menuItem("Advanced options",
            tabName = "advanced",
            implantInputAdvancedUI("RemovalReason"),
            style = "padding-bottom:6px;"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Overview Removals'",
        implantInputGeneralUI("RemovalReason")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Survival analysis'",
        sidebarMenu(menuItem("Basic options",
          tabName = "basic",
          survivalPlotBasicInputUI("SurvivalPlot"),
          startExpanded = T,
          style = "padding-bottom:6px;"
        )
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Survival analysis'",
        sidebarMenu(
          menuItem("Advanced options",
            tabName = "advanced",
            survivalPlotAdvancedInputUI("SurvivalPlot"),
            style = "padding-bottom:6px;"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Survival analysis'",
        survivalPlotGeneralInputUI("SurvivalPlot")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Survival Factors'",
        sidebarMenu(
          menuItem("Basic options",
            tabName = "basic",
            factorSurvivalPlotBasicInputUI("FactorSurvival"),
            startExpanded = T,
            style = "padding-bottom:6px;"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Survival Factors'",
        factorSurvivalPlotGeneralInputUI("FactorSurvival")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer'",
        sidebarMenu(
          menuItem("Options",
            tagList(
              explorerPlotOptionsUI("Explorer"),
              materialSwitch(
                inputId = "useAdvanced",
                label = "Advanced Mode",
                value = F,
                status = "primary"
              )
            ),
            startExpanded = T,
            style = "padding-bottom:6px;"
          )
        )
      ),
      conditionalPanel(
        condition = "input.tabs == 'Explorer'",
        explorerPlotGeneralOptionsUI("Explorer")
      )
    ),
    sidebarMenu(
      conditionalPanel(
        condition = "input.tabs == 'Analyses'",
        analyzePlotInputUI("Analyze")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Clinic'",
        clinicInputUI("ClinicInfo")
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
          style = "margin-top:10px;"
        )
      ),
      tabPanel(
        "Survival analysis",
        fluidRow(
          box(
            title = "Survival Analysis",
            survivalPlotUI("SurvivalPlot"),
            width = 12
          ),
          style = "margin-top:10px;"
        ),
      ),
      tabPanel(
        "Survival Factors",
        fluidRow(
          box(
            title = "Survival by factor",
            factorSurvivalPlotUI("FactorSurvival"),
            width = 6
          ),
          column(
            box(
              title = "Drilldown options",
              factorImplantSurvivalPlotInputUI("FactorSurvival"),
              width = 12
            ),
            box(
              title = "Drilldown Implant",
              factorImplantSurvivalPlotUI("FactorSurvival"),
              width = 12
            ),
            width = 6
          ),
          style = "margin-top:10px;"
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
          style = "margin-top:10px;"
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
          style = "margin-top:10px;"
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
          style = "margin-top:10px;"
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
  overallFilter <- reactiveVal("None")
  overallFilterLevels <- reactiveVal(NULL)
  useAdvanced <- reactiveVal(FALSE)

  # ---------------------------------------------------------------------------
  # Global events and variables
  # ---------------------------------------------------------------------------

  plotsInReport <- reactiveValues()

  output$overallFilterLevels <- renderUI({
    req(isTruthy(input$overallFilter),
        isTruthy(input$overallFilter != "None"))

    pickerInput("overallFilterLevels",
      paste0("Filter ", input$overallFilter),
      choices = as.character(
        sort(unique(completeTable[[input$overallFilter]]))
      ),
      multiple = T,
      options = list(
        `actions-box` = TRUE, size = 10,
        `selected-text-format` = "count > 2"
      )
    )
  })

  observeEvent(input$overallFilter, ignoreNULL = F, {
    overallFilter(input$overallFilter)
  })

  observeEvent(input$overallFilterLevels, ignoreNULL = F, {
    overallFilterLevels(input$overallFilterLevels)
  })

  observeEvent(input$useAdvanced, {
    useAdvanced(input$useAdvanced)
  })


  # ---------------------------------------------------------------------------
  # Removals Plot
  # ---------------------------------------------------------------------------

  implantServer("RemovalReason",
    data = completeTable, plotsInReport,
    overallFilter = overallFilter,
    overallFilterLevels = overallFilterLevels
  )

  #----------------------------------------------------------------------------
  # Explorer plot
  #----------------------------------------------------------------------------

  explorerPlotServer("Explorer",
    data = completeTable,
    plotInReport = plotsInReport,
    overallFilter = overallFilter,
    overallFilterLevels = overallFilterLevels,
    useAdvanced = useAdvanced
  )

  #----------------------------------------------------------------------------
  # Analyses
  #----------------------------------------------------------------------------

  analyzePlotServer("Analyze", completeTable, plotInReport = plotsInReport)

  #----------------------------------------------------------------------------
  # Report
  #----------------------------------------------------------------------------

  reportServer("Report", data = completeTable, plotsInReport)

  stdReportServer("StdReport", data = completeTable)


  #----------------------------------------------------------------------------
  # Survival
  #----------------------------------------------------------------------------
  survivalPlotServer("SurvivalPlot",
    data = completeTable, plotInReport = plotsInReport,
    overallFilter = overallFilter,
    overallFilterLevels = overallFilterLevels
  )

  factorSurvivalServer("FactorSurvival",
    data = completeTable,
    plotInReport = plotsInReport,
    overallFilter = overallFilter,
    overallFilterLevels = overallFilterLevels
  )

  #----------------------------------------------------------------------------
  # Clinic
  #----------------------------------------------------------------------------
  clinicServer("ClinicInfo",
    data = completeTable, plotInReport = plotsInReport,
    overallFilter = overallFilter,
    overallFilterLevels = overallFilterLevels
  )
}

shinyApp(ui, server)

