reportSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("slider"), "Slider", 1, 100, 50),
    downloadButton(ns("report"), "Generate report")
  )
}

reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectYAxis")),
    htmlOutput(ns("selectXAxis")),
    actionBttn(ns("add"), "Add", style = "material-flat", color = "success"),
    verbatimTextOutput(ns('argList'))
  )
}


reportServer <- function(id, data, 
                         optionsToRemove,
                         allCombined,
                         selectedClinic,
                         clinicCompare,
                         myValues) {
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
      
      observe({
        if(input$add > 0 & !(is.null(input$selectYAxis) | is.null(input$selectXAxis))){
          myValues$dList <- c(isolate(myValues$dList),
                              list(c("y" = isolate(input$selectYAxis),
                                     "x" = isolate(input$selectXAxis),
                                     "allCombined" = isolate(allCombined()),
                                     "selectedClinic" = isolate(selectedClinic()),
                                     "clinicCompare" = isolate(clinicCompare()))))
        }
      })
      
      output$argList<-renderPrint({
        print(myValues$dList)
      })
      
      ### Y-axis
      output$selectYAxis <- renderUI({
        pickerInput(ns("selectYAxis"),
                    "Select Y-axis",
                    choices = c(numericOptions, logicalOptions, "Antall"),
                    selected = "Complications"
        )
      })
      
      ### X-axis
      output$selectXAxis <- renderUI({
        pickerInput(ns("selectXAxis"),
                    "Select X-axis",
                    choices = c(factorOptions, numericOptions),
                    selected = "Clinic"
        )
      })
      
      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("shiny-app/report.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(n = input$slider, plotArgs = myValues$dList, data = data)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
    })
}
