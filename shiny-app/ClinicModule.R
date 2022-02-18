source("shiny-app/ClinicSuccessRatePlot.R")
source("shiny-app/ClinicTimeSeries.R")
source("shiny-app/ClinicInfoBoxes.R")
source("shiny-app/ClinicToothImplantPlot.R")


clinicInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectClinic"), width = 12),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

clinicSuccessRatePlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("successRatePlot"), height = 350) %>% withSpinner()
}

clinicAntibioticUsagePlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("antibioticUsagePlot"), height = 200) %>% withSpinner()
}

clinicInfectionPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("infectionPlot"), height = 200) %>% withSpinner()
}

clincToothImplantPlotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("toothImplants"), height = 350) %>% withSpinner()
}

clinicAntibioticInfoUI <- function(id) {
  ns <- NS(id)
  infoBoxOutput(ns("antibioticsInfo"), width = 12) %>% withSpinner()
}

complicationsInfoUI <- function(id) {
  ns <- NS(id)
  infoBoxOutput(ns("complicationsInfo"), width = 12) %>% withSpinner()
}

guiderailInfoUI <- function(id) {
  ns <- NS(id)
  infoBoxOutput(ns("guiderailInfo"), width = 12) %>% withSpinner()
}



clinicServer <- function(id, data, plotInReport) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(input$add, {
        # Show a modal when the button is pressed
        shinyalert::shinyalert(
          title = "Confirm",
          text = "Do you want to add this graph to the report?",
          size = "s", 
          closeOnEsc = TRUE,
          closeOnClickOutside = FALSE,
          html = FALSE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = TRUE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          cancelButtonText = "Cancel",
          timer = 0,
          imageUrl = "",
          animation = TRUE,
          inputId = "confirm"
        )
      })
      
      observeEvent(input$confirm, {
        if(input$confirm){
          plotInReport$dList <- c(isolate(plotInReport$dList),
                                  list(c("Factor" = isolate(input$selectFactor),
                                         "Levels" = isolate(paste(input$selectLevels, collapse = ";")),
                                         "AdditionalFactor" = isolate(input$selectAdditionalFactor),
                                         "AdditionalLevels" = isolate(paste(input$selectAdditionalLevels, collapse = ";")),
                                         "tab" = "Implant Survival")))
        }
      })
      
      
      ## Inputs -------------------------------------------------------------------
      output$selectClinic <- renderUI({
        pickerInput(ns("selectClinic"),
                    "Select Clinic",
                    choices = as.character(
                      sort(unique(data[["InsertionClinic"]]))
                    ),
                    selected = "None"
        )
      })
      
      
      ## Plots --------------------------------------------------------------------
      output$antibioticsInfo <- renderValueBox({
        antibioticsInfo(data, input$selectClinic)
      })
      
      output$complicationsInfo <- renderValueBox({
        complicationsInfo(data, input$selectClinic)
      })
      
      output$guiderailInfo <- renderValueBox({
        guiderailInfo(data, input$selectClinic)
      })
      
      output$successRatePlot <- renderPlot({
        clinicSuccesRatePlot(data = data, selectedClinic = input$selectClinic)
      })
      
      output$antibioticUsagePlot <- renderPlot({
        abPredicate <- function (dat, col1 = "AntibioticsDoseMg"){
          !is.na(dat[[col1]])
        }
        
        clinicTimeSeriesPlot(data = data,
                            selectedClinic = input$selectClinic,
                            insertionsOrRemovals = "Insertions",
                            predicate = abPredicate,
                            xlab = "Year",
                            ylab = "Patients recieved\n antibiotics %")
      })
      
      output$infectionPlot <- renderPlot({
        rrPredicate <- function (dat, col1 = "RemovalReason", col2 = "YearsSinceInsertion"){
          dat[[col1]] == "Infeksjon" & dat[[col2]] < 1
        }
        
        clinicTimeSeriesPlot(data = data,
                             selectedClinic = input$selectClinic,
                             insertionsOrRemovals = "Removals",
                             predicate = rrPredicate,
                             xlab = "Year",
                             ylab = "% of first year removals\n due to infection")
      })
      
      output$toothImplants <- renderPlot({
        clincToothImplantPlot(data, input$selectClinic)
      })
    }
  )
}
