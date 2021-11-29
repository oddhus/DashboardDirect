stdReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectClinic"), width = 12),
    downloadButton(ns("stdReport"), "Generate standard report")
  )
}


stdReportServer <- function(id, insertions,
                         removals,
                         myValues) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      clinics <- removals %>%
        distinct(Clinic) %>%
        arrange(Clinic) %>%
        mutate(Clinic = as.character(Clinic))
      
      output$selectClinic <- renderUI({
        pickerInput(ns("selectClinic"),
                    "Select Clinic",
                    choices = clinics
        )
      })
      
      output$stdReport <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "stdReport.html",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "stdReport.Rmd")
          file.copy("shiny-app/stdReport.Rmd", tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(clinic = input$selectClinic, insertions = insertions, removals = removals)
          
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
