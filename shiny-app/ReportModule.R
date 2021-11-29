reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("argList")),
    downloadButton(ns("report"), "Generate report")
  )
}


reportServer <- function(id, insertions, removals, myValues) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$argList <- renderPrint({
        print(myValues$dList)
      })

      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
          withProgress(
            message = paste0("Downloading custom report"),
            value = 0,
            {
              shiny::incProgress(1 / 10)
              # Copy the report file to a temporary directory before processing it, in
              # case we don't have write permissions to the current working dir (which
              # can happen when deployed).
              tempReport <- file.path(tempdir(), "report.Rmd")
              file.copy("shiny-app/report.Rmd", tempReport, overwrite = TRUE)

              shiny::incProgress(2 / 10)
              # Set up parameters to pass to Rmd document
              params <- list(
                plotArgs = myValues$dList,
                insertions = insertions, removals = removals
              )

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
