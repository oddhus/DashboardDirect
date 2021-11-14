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
#source("shiny-app/ClinicInsertionsInfo.R")
source("shiny-app/ImplantPlot.R")
#source("shiny-app/ExplorerPlotInputs.R")
source("shiny-app/ExplorerPlot.R")
source("shiny-app/ClinicInfoBoxes.R")

#Todo
#Add features shinydashboardPlus

ui <- dashboardPage(
    dashboardHeader(title = "Tooth implants"),
    dashboardSidebar(
      sidebarMenu(
        conditionalPanel(condition = "input.tabs == 'Clinic'",
                         radioGroupButtons(
                           inputId = "insertionsOrRemovals",
                           label = "Type", 
                           choices = c("Insertions", "Removals"),
                           status = "primary"
                         ),
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
                         htmlOutput("selectInsertionsFactorLevels", width = 12),
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
        conditionalPanel(condition = "input.tabs == 'Clinic",
                         checkboxGroupButtons(
                           inputId = "showMeanAndXLab",
                           label = "Show",
                           choices = c("x-lab", "Mean"),
                           status = "info",
                           selected = "x-lab"
                         ))
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
        tabPanel(
          "Explorer",
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

columnsToRemove <- c("RefNr", "Id.y", "Id", "ComplicationsComment", "AntibioticsType")

facetRowsColumnsToRemove <- c("LotNr")


server <- function(input, output, session) {
  insertionsWithImplants <- getInsertionsWithImplants()
  removalsWithImplants <- getRemovalsWithImplants()
  
  insertionsFactors <- insertionsWithImplants %>% select(where(is.factor) & !any_of(columnsToRemove)) %>% names()
  insertionsLogical <- insertionsWithImplants %>% select(where(is.logical) & !any_of(columnsToRemove)) %>% names()
  insertionsNumeric <- insertionsWithImplants %>% select(where(is.numeric) & !any_of(columnsToRemove)) %>% names()
  
  removalsFactors <- removalsWithImplants %>% select(where(is.factor) & !any_of(columnsToRemove)) %>% names()
  removalsLogical <- removalsWithImplants %>% select(where(is.logical) & !any_of(columnsToRemove)) %>% names()
  removalsNumeric <- removalsWithImplants %>% select(where(is.numeric) & !any_of(columnsToRemove)) %>% names()
  
  clinics <- insertionsWithImplants %>% distinct(Clinic) %>% arrange(Clinic) %>% 
    mutate(Clinic = as.character(Clinic))
  
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

  #Tab 1 ----------------------------------------------------------------------
  # Clinic
  ranges <- reactiveValues(x = NULL)
  
  allCombined <- reactiveVal(FALSE)
  fillColor <- reactiveVal("Clinic")
  
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
  
  # Render Insertions or removals depending on the plot viewed by user
  output$clinicPlotName <- renderText({
    input$insertionsOrRemovals
  })
  
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
  
  # Hides the plot not currently viewed by user, avoiding to generate the hidden
  # plot again
  observeEvent(input$insertionsOrRemovals, {
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
  })

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
        `selected-text-format` = "count > 3"
      )
    )
  })
  
  ### Debounce clinc compare variable
  input_selectClinicCompare <- reactiveVal(NULL)
  
  #### Ensure that same value dont trigger rerender
  observeEvent(input$selectClinicCompare, {
    if(!identical(input_selectClinicCompare(), input$selectClinicCompare)){
      input_selectClinicCompare(input$selectClinicCompare)
    } 
  })
  
  #### Debounce time
  input_selectClinicCompare_d <- input_selectClinicCompare %>% debounce(900)
  
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
  
  ## Insertions
  output$selectInsertionsFacetRow <- renderUI({
    pickerInput("selectInsertionsFacetRowControl",
                "Select Facet Row",
                choices = c(insertionsFactors[insertionsFactors != "LotNr"],
                            insertionsLogical, "None"),
                selected = "None"
    )
  })

  output$selectSpecificInsertionsFacetRow <- renderUI({
    if (isTruthy(input$selectInsertionsFacetRowControl) &
        isTruthy(input$selectInsertionsFacetRowControl != "None")) {
      selectedFactor <- input$selectXAxisInsertionsControl
      pickerInput("selectSpecificInsertionsFacetRowControl",
                  label = paste0("Select", selectedFactor),
                  choices = as.character(
                    sort(unique(insertionsWithImplants[[selectedFactor]]))
                    ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    } 
  })

  output$selectInsertionsFactorLevels <- renderUI({
    if (isTruthy(input$selectXAxisInsertionsControl) &
        (isTruthy(input$selectXAxisInsertionsControl != "Clinic") | allCombined())) {
      selectedFactor <- input$selectXAxisInsertionsControl
      pickerInput("selectInsertionsFactorLevelsControl",
                  label = paste0("Select", selectedFactor),
                  choices = as.character(
                    sort(unique(insertionsWithImplants[[selectedFactor]]))
                    ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    }
  })

  output$selectYAxisInsertions <- renderUI({
    pickerInput("selectYAxisInsertionsControl",
                "Select Y-axis",
                choices = c(insertionsNumeric, insertionsLogical, "Antall"),
                selected = "Complications"
    )
  })

  output$selectXAxisInsertions <- renderUI({
    pickerInput("selectXAxisInsertionsControl",
                "Select X-axis",
                choices = insertionsFactors,
                selected = "Clinic"
    )
  })

  output$insertionsPlot <- renderPlot({
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

  ## Removals
  output$selectRemovalsFacetRow <- renderUI({
    pickerInput("selectRemovalsFacetRowControl",
                "Select Facet Row",
                choices = c(removalsFactors[removalsFactors != "LotNr"],
                            removalsLogical, "None"),
                selected = "None"
    )
  })
  
  output$selectSpecificRemovalsFacetRow <- renderUI({
    if (isTruthy(input$selectRemovalsFacetRowControl) &
        isTruthy(input$selectRemovalsFacetRowControl != "None")) {
      selectedRow <- input$selectRemovalsFacetRowControl
      pickerInput("selectSpecificRemovalsFacetRowControl",
                  label = paste0("Select", selectedRow),
                  choices = as.character(
                    sort(unique(removalsWithImplants[[selectedRow]]))
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
      selectedFactor <- input$selectXAxisRemovalsControl
      pickerInput("selectRemovalsFactorLevelsControl",
                  # Reactive label.
                  label = paste0("Select", selectedFactor),
                  # Reactive choices.
                  choices = as.character(
                    sort(unique(removalsWithImplants[[selectedFactor]]))
                    ),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE, size = 10,
                                 `selected-text-format` = "count > 2")
      )
    }
  })
  
  output$selectYAxisRemovals <- renderUI({
    pickerInput("selectYAxisRemovalsControl",
                "Select Y-axis",
                choices = c(removalsNumeric, removalsLogical, "Antall"),
                selected = "Complications"
    )
  })
  
  output$selectXAxisRemovals <- renderUI({
    pickerInput("selectXAxisRemovalsControl",
                "Select X-axis",
                choices = c(removalsFactors),
                selected = "Clinic"
    )
  })
  
  output$removalsPlot <- renderPlot({
    req(exists("removalsWithImplants"),
        isTruthy(input$selectYAxisInsertionsControl),
        isTruthy(input$selectXAxisInsertionsControl))
    
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

