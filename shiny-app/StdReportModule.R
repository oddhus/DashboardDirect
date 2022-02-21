stdReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectClinic"), width = 12),
    downloadButton(ns("stdReport"), "Generate standard report")
  )
}


stdReportServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$selectClinic <- renderUI({
        pickerInput(ns("selectClinic"),
          "Select Clinic",
          choices = data %>%
            distinct(InsertionClinic) %>%
            arrange(InsertionClinic) %>%
            mutate(InsertionClinic = as.character(InsertionClinic))
        )
      })

      output$stdReport <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "stdReport.html",
        content = function(file) {
          withProgress(
            message = paste0("Downloading report"),
            value = 0,
            {
              shiny::incProgress(1 / 10)
              # Copy the report file to a temporary directory before processing it, in
              # case we don't have write permissions to the current working dir (which
              # can happen when deployed).
              tempReport <- file.path(tempdir(), "stdReport.Rmd")
              file.copy("shiny-app/stdReport.Rmd", tempReport, overwrite = TRUE)

              shiny::incProgress(2 / 10)
              # Set up parameters to pass to Rmd document
              params <- list(clinic = input$selectClinic, data = data)

              shiny::incProgress(5 / 10)
              # Knit the document, passing in the `params` list, and eval it in a
              # child of the global environment (this isolates the code in the document
              # from the code in this app).
              rmarkdown::render(tempReport,
                output_file = file,
                params = params,
                envir = new.env(parent = globalenv())
              )
            }
          )
        }
      )
    }
  )
}
