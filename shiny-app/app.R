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
source("shiny-app/ImplantPlot.R")
source("shiny-app/ExplorerPlot.R")
source("shiny-app/ClinicInfoBoxes.R")
source("shiny-app/LogRegPlot.R")


#Todo
#Add features shinydashboardPlus

ui <- dashboardPage(
    dashboardHeader(title = "Tooth implants"),
    dashboardSidebar(
      sidebarMenu(
        conditionalPanel(condition = "input.tabs == 'Clinic' | input.tabs == 'Analyses'",
                         radioGroupButtons(
                           inputId = "insertionsOrRemovals",
                           label = "Type", 
                           choices = c("Insertions", "Removals"),
                           status = "primary")
                          ),
        conditionalPanel(condition = "input.tabs == 'Clinic'",   
                         radioGroupButtons(
                           inputId = "clinicOrAllcombined",
                           label = "Grouping",
                           choices = c("Clinic", "All combined"),
                           status = "primary" )
                         ),
        conditionalPanel(condition = "input.tabs == 'Clinic' & input.clinicOrAllcombined == 'Clinic'",
                         htmlOutput("selectClinicName"),
                         htmlOutput("selectClinicCompare", width = 12)
                         ),
        conditionalPanel(condition = "input.tabs == 'Clinic' & input.insertionsOrRemovals == 'Insertions'",
                         htmlOutput("selectYAxisInsertions", width = 12),
                         htmlOutput("selectXAxisInsertions", width = 12),
                         htmlOutput("selectInsertionsFactorLevels", width = 12)
                         ),
        conditionalPanel(condition = "input.tabs == 'Clinic' & input.insertionsOrRemovals == 'Removals'",
                         htmlOutput("selectYAxisRemovals", width = 12),
                         htmlOutput("selectXAxisRemovals", width = 12),
                         htmlOutput("selectRemovalsFactorLevels", width = 12) 
                         ),
        conditionalPanel(condition = "input.tabs == 'Clinic' & input.clinicOrAllcombined == 'All combined'",
                         htmlOutput("selectFillColor", width = 12)
                         ),
        conditionalPanel(condition = "input.tabs == 'Clinic' & input.insertionsOrRemovals == 'Insertions'",
                        htmlOutput("selectInsertionsFacetRow", width = 12),
                        htmlOutput("selectSpecificInsertionsFacetRow", width = 12)
                        ),
        conditionalPanel(condition = "input.tabs == 'Clinic' & input.insertionsOrRemovals == 'Removals'",
                         htmlOutput("selectRemovalsFacetRow", width = 12),
                         htmlOutput("selectSpecificRemovalsFacetRow", width = 12) 
                         ),
        conditionalPanel(condition = "input.tabs == 'Clinic'",
                         checkboxGroupButtons(
                           inputId = "showMeanAndXLab",
                           label = "Show",
                           choices = c("x-lab", "Mean"),
                           status = "info",
                           selected = "x-lab"
                         ))
        ),
        conditionalPanel(condition = "input.tabs == 'Analyses'",
                         pickerInput("analyzeMethod",
                                     label = "Select analysis",
                                     choices = c("Logistic Regression"),
                                     selected = "Logistic Regression" )
                         ),
        conditionalPanel(condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Insertions' & input.analyzeMethod == 'Logistic Regression'",
                         htmlOutput("logRegDependentInsertions"),
                         htmlOutput("logRegNumericIndependentInsertions"),
                         htmlOutput("logRegFactorLogicalIndependentInsertions"),
                         ),
        conditionalPanel(condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Removals' & input.analyzeMethod == 'Logistic Regression'",
                         htmlOutput("logRegDependentRemovals"),
                         htmlOutput("logRegNumericIndependentRemovals"),
                         htmlOutput("logRegFactorLogicalIndependentRemovals"),
      )
      ),
    dashboardBody(
      shinyjs::useShinyjs(),
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Clinic",
          h2("Explore and compare clinic data"),
          h3(textOutput("clinicPlotName")),
          fluidRow(
            column(
              plotOutput("insertionsPlot", height = 500) %>% withSpinner(id = "insertionsSpinner"),
              plotOutput("removalsPlot", height = 500) %>% withSpinner(id = "removalsSpinner"),
              width = 12
            ),
          ),
          fluidRow(
            column(
              valueBoxOutput("insertionsBox", width = 4),
              valueBoxOutput("removalsBox", width = 4),
              valueBoxOutput("complicationBox", width = 4),
              width = 12
            )
          ),
        ),
        tabPanel(
          "Analyses",
          h2("Analyze data"),
          fluidRow(
            column(
              plotOutput("logRegPlotInsertions", height = 600) %>% withSpinner(id = "logRegInsertionsSpinner"),
              htmlOutput("logRegHighlightInsertions"),
              width = 6
            ),
            column(
              verbatimTextOutput("logRegPrintInsertions"), 
              width = 6
            ),
            column(
              plotOutput("logRegPlotRemovals", height = 600) %>% withSpinner(id = "logRegRemovalsSpinner"),
              htmlOutput("logRegHighlightRemovals"),
              width = 6
            ),
            column(
              verbatimTextOutput("logRegPrintRemovals"), 
              width = 6
            )
          )
        ),
        tabPanel(
          "Models",
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
  columnsToRemove <- c("RefNr", "Id.y", "Id", "ComplicationsComment", "AntibioticsType")
  facetRowsColumnsToRemove <- c("LotNr")
  
  # ---------------------------------------------------------------------------
  # Data 
  # ---------------------------------------------------------------------------
  
  insertionsWithImplants <- getInsertionsWithImplants()
  removalsWithImplants <- getRemovalsWithImplants()
  
  insertionsFactors <- insertionsWithImplants %>%
    select(where(is.factor) & !any_of(columnsToRemove)) %>% names()
  insertionsLogical <- insertionsWithImplants %>%
    select(where(is.logical) & !any_of(columnsToRemove)) %>% names()
  insertionsNumeric <- insertionsWithImplants %>%
    select(where(is.numeric) & !any_of(columnsToRemove)) %>% names()
  
  removalsFactors <- removalsWithImplants %>%
    select(where(is.factor) & !any_of(columnsToRemove)) %>% names()
  removalsLogical <- removalsWithImplants %>%
    select(where(is.logical) & !any_of(columnsToRemove)) %>% names()
  removalsNumeric <- removalsWithImplants %>% 
    select(where(is.numeric) & !any_of(columnsToRemove)) %>% names()
  
  clinics <- insertionsWithImplants %>% distinct(Clinic) %>% arrange(Clinic) %>% 
    mutate(Clinic = as.character(Clinic))
  
  # ---------------------------------------------------------------------------
  # Global events
  # ---------------------------------------------------------------------------
  
  # Hides the plot not currently viewed by user, avoiding to generate the hidden
  # plot again
  observeEvent(input$insertionsOrRemovals, {
    if(input$tabs == "Clinic"){
      if(input$insertionsOrRemovals == "Insertions") {
        shinyjs::show("insertionsSpinner")
        shinyjs::hide("removalsSpinner")
        shinyjs::show("insertionsPlot")
        shinyjs::hide("removalsPlot")
      } else if (input$insertionsOrRemovals == "Removals"){
        shinyjs::hide("insertionsSpinner")
        shinyjs::show("removalsSpinner")
        shinyjs::hide("insertionsPlot")
        shinyjs::show("removalsPlot")
      }
    } else {
      if(input$insertionsOrRemovals == "Insertions") {
        shinyjs::show("logRegInsertionsSpinner")
        shinyjs::hide("logRegRemovalsSpinner")
        shinyjs::show("logRegPlotInsertions")
        shinyjs::hide("logRegPlotRemovals")
        shinyjs::show("logRegPrintInsertions")
        shinyjs::hide("logRegPrintRemovals")
      } else if (input$insertionsOrRemovals == "Removals"){
        shinyjs::hide("logRegInsertionsSpinner")
        shinyjs::show("logRegRemovalsSpinner")
        shinyjs::hide("logRegPlotInsertions")
        shinyjs::show("logRegPlotRemovals")
        shinyjs::hide("logRegPrintInsertions")
        shinyjs::show("logRegPrintRemovals")
      }
    }

  })
  
  
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
      choices  = unique(insertionsWithImplants$ImplantName),
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

  #----------------------------------------------------------------------------
  # Explorer plot
  #----------------------------------------------------------------------------
  
  ## Reactive vals ------------------------------------------------------------
  
  ranges <- reactiveValues(x = NULL)
  allCombined <- reactiveVal(FALSE)
  fillColor <- reactiveVal("Clinic")
  input_selectClinicCompare <- reactiveVal(NULL)
  
  ## Observe events -----------------------------------------------------------
  
  # Update relevant values when switching from clinic to all combined mode
  observeEvent(input$clinicOrAllcombined, {
    if ("Clinic" %in% input$clinicOrAllcombined){
      fillColor("Clinic")
      allCombined(FALSE)
    } else {
      allCombined(TRUE)
      fillColor("None")
      #Update the picker to reflect the current value
      updatePickerInput(session, "selectFillColorControl", selected = "None")
    }
  })
  
  #Only change fill color when in all combined
  observeEvent(input$selectFillColorControl, {
    if("All combined" %in% input$clinicOrAllcombined){
      fillColor(input$selectFillColorControl)
    }
  })
  
  observeEvent(input$selectClinicCompare, {
    #### Ensure that same value dont trigger rerender
    if(!identical(input_selectClinicCompare(), input$selectClinicCompare)){
      input_selectClinicCompare(input$selectClinicCompare)
    } 
  })
  
  ## Debounce -----------------------------------------------------------------
  ### Debounce clinc compare variable
  input_selectClinicCompare_d <- input_selectClinicCompare %>% debounce(900)
  
  ## Input --------------------------------------------------------------------
  
  ### Clinics
  output$selectClinicName <- renderUI({
    pickerInput("selectClinic",
                "Select Clinic",
                choices = clinics
    )
  })
  
  output$selectClinicCompare <- renderUI({
    req(input$selectClinic)
    
    pickerInput("selectClinicCompare",
                "Compare Clinics",
                # Do not compare against the current selected clinic
                choices = clinics %>% filter(Clinic != input$selectClinic),
                multiple = T,
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 2"
                )
    )
  })
  
  ### Y-axis
  output$selectYAxisInsertions <- renderUI({
    pickerInput("selectYAxisInsertionsControl",
                "Select Y-axis",
                choices = c(insertionsNumeric, insertionsLogical, "Antall"),
                selected = "Complications"
    )
  })
  
  output$selectYAxisRemovals <- renderUI({
    pickerInput("selectYAxisRemovalsControl",
                "Select Y-axis",
                choices = c(removalsNumeric, removalsLogical, "Antall"),
                selected = "Complications"
    )
  })
  
  ### X-axis
  output$selectXAxisInsertions <- renderUI({
    pickerInput("selectXAxisInsertionsControl",
                "Select X-axis",
                choices = insertionsFactors,
                selected = "Clinic"
    )
  })
  
  output$selectXAxisRemovals <- renderUI({
    pickerInput("selectXAxisRemovalsControl",
                "Select X-axis",
                choices = c(removalsFactors),
                selected = "Clinic"
    )
  })
  
  ### Fill color
  output$selectFillColor <- renderUI({
    pickerInput("selectFillColorControl",
                "Select Fill Color",
                choices = if(input$insertionsOrRemovals == "Insertions") {
                  c(insertionsFactors[insertionsFactors != "LotNr"],
                    insertionsLogical, "None")
                } else {
                  c(removalsFactors[removalsFactors != "LotNr"],
                    removalsLogical, "None")
                },
                selected = "None"
    )
  })
  
  ### X-axis levels
  output$selectInsertionsFactorLevels <- renderUI({
    if (isTruthy(input$selectXAxisInsertionsControl) &
        (isTruthy(input$selectXAxisInsertionsControl != "Clinic") | allCombined())) {
      pickerInput("selectInsertionsFactorLevelsControl",
                  label = paste0("Select ", input$selectXAxisInsertionsControl),
                  choices = as.character(
                    sort(unique(insertionsWithImplants[[input$selectXAxisInsertionsControl]]))
                  ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    }
  })
  
  output$selectRemovalsFactorLevels <- renderUI({
    if (isTruthy(input$selectXAxisRemovalsControl) &
        (isTruthy(input$selectXAxisRemovalsControl != "Clinic") | allCombined())) {
      pickerInput("selectRemovalsFactorLevelsControl",
                  # Reactive label.
                  label = paste0("Select ", input$selectXAxisRemovalsControl),
                  # Reactive choices.
                  choices = as.character(
                    sort(unique(removalsWithImplants[[input$selectXAxisRemovalsControl]]))
                  ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    }
  })
  
  ### Facet row
  output$selectInsertionsFacetRow <- renderUI({
    pickerInput("selectInsertionsFacetRowControl",
                "Select Facet Row",
                choices = c(insertionsFactors[insertionsFactors != "LotNr"],
                            insertionsLogical, "None"),
                selected = "None"
    )
  })
  
  output$selectRemovalsFacetRow <- renderUI({
    pickerInput("selectRemovalsFacetRowControl",
                "Select Facet Row",
                choices = c(removalsFactors[removalsFactors != "LotNr"],
                            removalsLogical, "None"),
                selected = "None"
    )
  })
  
  ### Specific facet row
  output$selectSpecificInsertionsFacetRow <- renderUI({
    if (isTruthy(input$selectInsertionsFacetRowControl) &
        isTruthy(input$selectInsertionsFacetRowControl != "None")) {
      pickerInput("selectSpecificInsertionsFacetRowControl",
                  label = paste0("Select ", input$selectInsertionsFacetRowControl),
                  choices = as.character(
                    sort(unique(insertionsWithImplants[[input$selectInsertionsFacetRowControl]]))
                    ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    } 
  })

  output$selectSpecificRemovalsFacetRow <- renderUI({
    if (isTruthy(input$selectRemovalsFacetRowControl) &
        isTruthy(input$selectRemovalsFacetRowControl != "None")) {
      pickerInput("selectSpecificRemovalsFacetRowControl",
                  label = paste0("Select ", selectRemovalsFacetRowControl),
                  choices = as.character(
                    sort(unique(removalsWithImplants[[selectRemovalsFacetRowControl]]))
                  ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    }
  })

  ## Plots -------------------------------------------------------------------
  
  # Render Insertions or removals depending on the plot viewed by user
  output$clinicPlotName <- renderText({
    input$insertionsOrRemovals
  })
  
  output$insertionsPlot <- renderPlot({
    req(exists("removalsWithImplants"),
        isTruthy(input$selectYAxisInsertionsControl),
        isTruthy(input$selectXAxisInsertionsControl))
    
    exploreDataPlot(
      insertionsWithImplants,
      "Mean" %in% input$showMeanAndXLab,
      !("x-lab" %in% input$showMeanAndXLab),
      input$selectClinic,
      input_selectClinicCompare_d(),
      fillColor(),
      input$selectInsertionsFacetRowControl,
      input$selectSpecificInsertionsFacetRowControl,
      input$selectInsertionsFactorLevelsControl,
      input$selectYAxisInsertionsControl,
      input$selectXAxisInsertionsControl,
      allCombined()
    )
  })
  
  output$removalsPlot <- renderPlot({
    req(exists("removalsWithImplants"),
        isTruthy(input$selectYAxisRemovalsControl),
        isTruthy(input$selectXAxisRemovalsControl))
    
    exploreDataPlot(
      removalsWithImplants,
      "Mean" %in% input$showMeanAndXLab,
      !("x-lab" %in% input$showMeanAndXLab),
      input$selectClinic,
      input_selectClinicCompare_d(),
      fillColor(),
      input$selectRemovalsFacetRowControl,
      input$selectSpecificRemovalsFacetRowControl,
      input$selectRemovalsFactorLevelsControl,
      input$selectYAxisRemovalsControl,
      input$selectXAxisRemovalsControl,
      allCombined()
    )
  })
  
  #3 Info-boxes --------------------------------------------------------------
  output$complicationBox <- renderValueBox({
    sumInfo(insertionsWithImplants %>% filter(Complications),
            "Complications", "red", input$selectClinic, allCombined())
  })
  
  output$insertionsBox <- renderValueBox({
    sumInfo(insertionsWithImplants,
            "Insertions", "purple", input$selectClinic, allCombined())
  })
  
  output$removalsBox <- renderValueBox({
    sumInfo(removalsWithImplants,
            "Removals", "yellow", input$selectClinic, allCombined())
  })
  
  #----------------------------------------------------------------------------
  # Analyses
  #----------------------------------------------------------------------------
  
  # Logistic Regression -------------------------------------------------------
  
  ## Inputs -------------------------------------------------------------------
  
  ### Dependent variable
  output$logRegDependentInsertions <- renderUI({
    pickerInput("logRegDependentInsertions",
      label = "Select dependent variable",
      choices = insertionsLogical
    )
  })
  
  output$logRegDependentRemovals <- renderUI({
    pickerInput("logRegDependentRemovals",
                label = "Select dependent variable",
                choices = removalsLogical
    )
  })
  
  ### Independent variables
  output$logRegNumericIndependentInsertions <- renderUI({
    pickerInput("logRegNumericIndependentInsertions",
                label = "Select independent variable(s)",
                choices = insertionsNumeric,
                multiple = TRUE
    )
  })
  
  output$logRegNumericIndependentRemovals <- renderUI({
    pickerInput("logRegNumericIndependentRemovals",
                label = "Select independent variable(s)",
                choices = removalsNumeric,
                multiple = TRUE
    )
  })
  
  ### Additional independent variables
  output$logRegFactorLogicalIndependentInsertions <- renderUI({
    pickerInput("logRegFactorLogicalIndependentInsertions",
                label = "Select additional independent factors",
                choices = c(insertionsFactors, insertionsLogical),
                multiple = TRUE
    )
  })
  
  output$logRegFactorLogicalIndependentRemovals <- renderUI({
    pickerInput("logRegFactorLogicalIndependentRemovals",
                label = "Select additional independent factors",
                choices = c(removalsFactors, removalsLogical),
                multiple = TRUE
    )
  })
  
  ### Highlight
  output$logRegHighlightInsertions <- renderUI({
    req(isTruthy(input$logRegFactorLogicalIndependentInsertions))
    
    pickerInput("logRegHighlightInsertions",
                label = "Hightlight factor",
                choices = as.character(
                  sort(unique(insertionsWithImplants[[input$logRegFactorLogicalIndependentInsertions[1]]]))
                ),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10,
                               `selected-text-format` = "count > 2")
    )
  })
  
  output$logRegHighlightRemovals <- renderUI({
    req(isTruthy(input$logRegFactorLogicalIndependentRemovals))
    
    pickerInput("logRegHighlightRemovals",
                label = "Hightlight factor",
                choices = as.character(
                  sort(unique(removalsWithImplants[[input$logRegFactorLogicalIndependentRemovals[1]]]))
                ),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10,
                               `selected-text-format` = "count > 2")
    )
  })
  
  ## Plots -------------------------------------------------------------------
  
  output$logRegPlotInsertions <- renderPlot({
    req(isTruthy(input$logRegDependentInsertions),
        isTruthy(input$logRegNumericIndependentInsertions))
    
    logRegPlot(insertionsWithImplants,
                 input$logRegDependentInsertions,
                 input$logRegNumericIndependentInsertions,
                 input$logRegFactorLogicalIndependentInsertions,
                 input$logRegHighlightInsertions
    )
  })
  
  output$logRegPlotRemovals <- renderPlot({
    req(isTruthy(input$logRegDependentRemovals),
        isTruthy(input$logRegNumericIndependentRemovals))
    
    logRegPlot(removalsWithImplants,
                 input$logRegDependentRemovals,
                 input$logRegNumericIndependentRemovals,
                 input$logRegFactorLogicalIndependentRemovals,
               input$logRegHighlightRemoval
    )
  })
  
  ## SummaryText --------------------------------------------------------------
  output$logRegPrintInsertions <- renderPrint({
    req(isTruthy(input$logRegDependentInsertions),
        isTruthy(input$logRegNumericIndependentInsertions))
    
    form <- paste0(input$logRegDependentInsertions, "~",
                   paste0(c(input$logRegNumericIndependentInsertions, 
                            input$logRegFactorLogicalIndependentInsertions)
                            ,collapse = "+"))
    print(paste0("Formula: ", form))
    logreg <-glm(as.formula(form),family=binomial(),data=insertionsWithImplants)
    print(summary(logreg))
  })
  
  output$logRegPrintRemovals <- renderPrint({
    req(isTruthy(input$logRegDependentRemovals),
        isTruthy(input$logRegNumericIndependentRemovals))
    
    form <- paste0(input$logRegDependentRemovals, "~",
                   paste0(c(input$logRegNumericIndependentRemovals, 
                            input$logRegFactorLogicalIndependentRemovals)
                          ,collapse = "+"))
    print(paste0("Formula: ", form))
    logreg <-glm(as.formula(form),family=binomial(),data=removalsWithImplants)
    print(summary(logreg))
  })
}

shinyApp(ui, server)

