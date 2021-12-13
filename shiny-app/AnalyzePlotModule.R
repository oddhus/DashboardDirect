source("shiny-app/AnalyzePlot.R")

analyzePlotInputUI <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(ns("analyzeMethod"),
      label = "Select analysis",
      choices = c(
        "Binary Logistic Regression",
        "Linear Model"
      ),
      selected = "Binary Logistic Regression"
    ),
    htmlOutput(ns("dependent")),
    htmlOutput(ns("numericIndependent")),
    htmlOutput(ns("factorIndependent")),
    htmlOutput(ns("logicalIndependent")),
    actionBttn(ns("add"), "Add to report", style = "bordered", color = "warning")
  )
}

analyzePlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plotAnalyze"), height = 550) %>% withSpinner(id = ns("analyzeSpinner")),
    htmlOutput(ns("highlight"))
  )
}

analyzePrintUI <- function(id) {
  ns <- NS(id)
  verbatimTextOutput(ns("printAnalyze"))
}

analyzePlotServer <- function(id, data, optionsToRemove, isVisible, plotInReport) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      factorOptions <- data %>%
        select(where(is.factor) & !any_of(optionsToRemove)) %>%
        names()
      logicalOptions <- data %>%
        select(where(is.logical) & !any_of(optionsToRemove)) %>%
        names()
      numericOptions <- data %>%
        select(where(is.numeric) & !any_of(optionsToRemove)) %>%
        names()


      observeEvent(isVisible(), {
        if (isVisible()) {
          shinyjs::show("analyzeSpinner")
          shinyjs::show("printAnalyze")
          shinyjs::show("plotAnalyze")
          shinyjs::show("highlight")
        } else {
          shinyjs::hide("analyzeSpinner")
          shinyjs::hide("printAnalyze")
          shinyjs::hide("plotAnalyze")
          shinyjs::hide("highlight")
        }
      })
      
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
                                  list(c("dependent" = isolate(input$dependent),
                                         "numericIndependent" = isolate(paste(input$numericIndependent, collapse = ";")),
                                         "factorIndependent" = isolate(paste(input$factorIndependent, collapse = ";")),
                                         "logicalIndependent" = isolate(paste(input$logicalIndependent, collapse = ";")),
                                         "highlight" = isolate(paste(input$highlight, collapse = ";")),
                                         "analyzeMethod" =  isolate(input$analyzeMethod),
                                         "dataset" = id,
                                         "tab" = "Analyze")))
        }
      })
      
      #----------------------------------------------------------------------------
      # Analyses
      #----------------------------------------------------------------------------

      # Logistic Regression -------------------------------------------------------

      ## Inputs -------------------------------------------------------------------

      ### Dependent variable
      output$dependent <- renderUI({
        pickerInput(ns("dependent"),
          label = "Select dependent variable",
          choices = if (input$analyzeMethod == "Binary Logistic Regression") {
            logicalOptions
          } else if (input$analyzeMethod == "Linear Model") {
            numericOptions
          }
        )
      })

      ### Numeric Independent variables
      output$numericIndependent <- renderUI({
        pickerInput(ns("numericIndependent"),
          label = "Select numeric variable(s)",
          choices = numericOptions,
          multiple = TRUE
        )
      })

      ### Factor independent variables
      output$factorIndependent <- renderUI({
        pickerInput(ns("factorIndependent"),
          label = "Select factors",
          choices = factorOptions,
          multiple = TRUE
        )
      })

      ### Logical independent variables
      output$logicalIndependent <- renderUI({
        pickerInput(ns("logicalIndependent"),
          label = "Select logical variables",
          choices = logicalOptions,
          multiple = TRUE
        )
      })

      ### Highlight
      output$highlight <- renderUI({
        req(
          isTruthy(input$factorIndependent)
        )

        pickerInput(ns("highlight"),
          label = "Hightlight factor",
          choices = as.character(
            sort(unique(data[[input$factorIndependent[1]]]))
          ),
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE, size = 10,
            `selected-text-format` = "count > 2"
          )
        )
      })

      ## Plots -------------------------------------------------------------------

      output$plotAnalyze <- renderPlot({
        req(
          isTruthy(input$dependent),
          isTruthy(input$numericIndependent)
        )

        analyzePlot(
          data,
          input$dependent,
          input$numericIndependent,
          input$factorIndependent,
          input$logicalIndependent,
          input$highlight,
          input$analyzeMethod
        )
      })

      ## SummaryText --------------------------------------------------------------

      output$printAnalyze <- renderPrint({
        form <- paste0(
          input$dependent, "~",
          paste0(c(
            input$numericIndependent,
            input$factorIndependent,
            input$logicalIndependent
          ),
          collapse = "+"
          )
        )
        print(paste0("Formula: ", form))

        if (input$analyzeMethod == "Binary Logistic Regression") {
          req(isTruthy(input$dependent))
          validate(
            need(
              isTruthy(input$numericIndependent) |
                isTruthy(input$factorsIndependent) |
                isTruthy(input$logicalIndependent),
              "Please select an independent variable"
            )
          )

          logreg <- glm(as.formula(form), family = binomial(), data = data)
          print(summary(logreg))
        } else if (input$analyzeMethod == "Linear Model") {
          req(isTruthy(input$dependent))
          validate(
            need(
              isTruthy(input$numericIndependent),
              "Please select an numeric independent variable"
            )
          )

          lm <- lm(as.formula(form), data = data)
          print(summary(lm))
        }
      })
    }
  )
}
