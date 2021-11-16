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
                                     choices = c("Binary Logistic Regression", 
                                                 "Linear Model"),
                                     selected = "Binary Logistic Regression" )
                         ),
        conditionalPanel(condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Insertions'",
                         htmlOutput("dependentInsertions"),
                         htmlOutput("numericIndependentInsertions"),
                         htmlOutput("factorIndependentInsertions"),
                         htmlOutput("logicalIndependentInsertions")
                         ),
        conditionalPanel(condition = "input.tabs == 'Analyses' & input.insertionsOrRemovals == 'Removals'",
                         htmlOutput("dependentRemovals"),
                         htmlOutput("numericIndependentRemovals"),
                         htmlOutput("factorIndependentRemovals"),
                         htmlOutput("logicalIndependentRemovals")
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
              plotOutput("plotAnalyzeInsertions", height = 600) %>% withSpinner(id = "analyzeInsertionsSpinner"),
              htmlOutput("highlightInsertions"),
              width = 6
            ),
            column(
              verbatimTextOutput("printAnalyzeInsertions"), 
              width = 6
            ),
            column(
              plotOutput("plotAnalyzeRemovals", height = 600) %>% withSpinner(id = "analyzeRemovalsSpinner"),
              htmlOutput("highlightRemovals"),
              width = 6
            ),
            column(
              verbatimTextOutput("printAnalyzeRemovals"), 
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
        shinyjs::show("plotAnalyzeInsertions")
        shinyjs::hide("plotAnalyzeRemovals")
        shinyjs::show("printAnalyzeInsertions")
        shinyjs::hide("printAnalyzeRemovals")
      } else if (input$insertionsOrRemovals == "Removals"){
        shinyjs::hide("analyzeInsertionsSpinner")
        shinyjs::show("analyzeRemovalsSpinner")
        shinyjs::hide("plotAnalyzeInsertions")
        shinyjs::show("plotAnalyzeRemovals")
        shinyjs::hide("printAnalyzeInsertions")
        shinyjs::show("printAnalyzeRemovals")
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
  output$dependentInsertions <- renderUI({
    pickerInput("dependentInsertions",
              label = "Select dependent variable",
              choices = if(input$analyzeMethod == "Binary Logistic Regression"){
                insertionsLogical
              } else if (input$analyzeMethod == "Linear Model") {
                insertionsNumeric
              }
    )
  })
  
  output$dependentRemovals <- renderUI({
    pickerInput("dependentRemovals",
                label = "Select dependent variable",
                choices = if(input$analyzeMethod == "Binary Logistic Regression"){
                  removalsLogical
                } else if (input$analyzeMethod == "Linear Model") {
                  removalsNumeric
                }
    )
  })
  
  ### Numeric Independent variables
  output$numericIndependentInsertions <- renderUI({
    pickerInput("numericIndependentInsertions",
                label = "Select numeric variable(s)",
                choices = insertionsNumeric,
                multiple = TRUE
    )
  })
  
  output$numericIndependentRemovals <- renderUI({
    pickerInput("numericIndependentRemovals",
                label = "Select numeric variable(s)",
                choices = removalsNumeric,
                multiple = TRUE
    )
  })
  
  ### Factor independent variables
  output$factorIndependentInsertions <- renderUI({
    pickerInput("factorIndependentInsertions",
                label = "Select factors",
                choices = insertionsFactors,
                multiple = TRUE
    )
  })
  
  output$factorIndependentRemovals <- renderUI({
    pickerInput("factorIndependentRemovals",
                label = "Select factors",
                choices = removalsFactors,
                multiple = TRUE
    )
  })
  
  ### Logical independent variables
  output$logicalIndependentInsertions <- renderUI({
    pickerInput("logicalIndependentInsertions",
                label = "Select logical variables",
                choices = insertionsLogical,
                multiple = TRUE
    )
  })
  
  output$logicalIndependentRemovals <- renderUI({
    pickerInput("logicalIndependentRemovals",
                label = "Select logical variables",
                choices = removalsLogical,
                multiple = TRUE
    )
  })
  
  ### Highlight
  output$highlightInsertions <- renderUI({
    req(isTruthy(input$factorIndependentInsertions))
    
    pickerInput("highlightInsertions",
                label = "Hightlight factor",
                choices = as.character(
                  sort(unique(insertionsWithImplants[[input$factorIndependentInsertions[1]]]))
                ),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10,
                               `selected-text-format` = "count > 2")
    )
  })
  
  output$highlightRemovals <- renderUI({
    req(isTruthy(input$factorIndependentRemovals))
    
    pickerInput("highlightRemovals",
                label = "Hightlight factor",
                choices = as.character(
                  sort(unique(removalsWithImplants[[input$factorIndependentRemovals[1]]]))
                ),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, size = 10,
                               `selected-text-format` = "count > 2")
    )
  })
  
  ## Plots -------------------------------------------------------------------
  
  output$plotAnalyzeInsertions <- renderPlot({
      req(isTruthy(input$dependentInsertions),
          isTruthy(input$numericIndependentInsertions))

      analyzePlot(insertionsWithImplants,
                 input$dependentInsertions,
                 input$numericIndependentInsertions,
                 input$factorIndependentInsertions,
                 input$logicalIndependentInsertions,
                 input$highlightInsertions,
                 input$analyzeMethod
      )
  })
  
  output$plotAnalyzeRemovals <- renderPlot({
    req(isTruthy(input$dependentRemovals),
        isTruthy(input$numericIndependentRemovals))
    
    analyzePlot(removalsWithImplants,
                 input$dependentRemovals,
                 input$numericIndependentRemovals,
                 input$factorIndependentRemovals,
                 input$logicalIndependentRemovals,
                 input$highlightRemovals,
                 input$analyzeMethod
                
    )
  })
  
  ## SummaryText --------------------------------------------------------------
  

  
  output$printAnalyzeInsertions <- renderPrint({


    form <- paste0(input$dependentInsertions, "~",
                   paste0(c(input$numericIndependentInsertions, 
                            input$factorIndependentInsertions,
                            input$logicalIndependentInsertions)
                          ,collapse = "+"))
    
    print(paste0("Formula: ", form))
    

    
    if(input$analyzeMethod == "Binary Logistic Regression"){
      req(isTruthy(input$dependentInsertions),
          isTruthy(input$numericIndependentInsertions) | isTruthy(input$factorIndependentInsertions))
      logreg <-glm(as.formula(form),family=binomial(),data=insertionsWithImplants)
      print(summary(logreg))
    } else if (input$analyzeMethod == "Linear Model") {
      req(isTruthy(input$dependentInsertions),
          (isTruthy(input$numericIndependentInsertions)))
      lm <- lm(as.formula(form), data = insertionsWithImplants)
      print(summary(lm))
    }
  })
  
  output$printAnalyzeRemovals <- renderPrint({
    form <- paste0(input$dependentRemovals, "~",
                   paste0(c(input$numericIndependentRemovals, 
                            input$factorIndependentRemovals,
                            input$logicalIndependentRemovals)
                          ,collapse = "+"))
    print(paste0("Formula: ", form))
    
    if(input$analyzeMethod == "Binary Logistic Regression"){
      req(isTruthy(input$dependentRemovals),
          (isTruthy(input$numericIndependentRemovals) | isTruthy(input$factorsIndependentRemovals)))
      
      logreg <-glm(as.formula(form),family=binomial(),data=removalsWithImplants)
      print(summary(logreg))
    } else if (input$analyzeMethod == "Linear Model") {
      req(isTruthy(input$dependentRemovals),
          (isTruthy(input$numericIndependentRemovals)))
      
      lm <- lm(as.formula(form), data = removalsWithImplants)
      print(summary(lm))
    }
  })
}

shinyApp(ui, server)

