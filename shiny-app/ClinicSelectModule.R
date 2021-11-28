clinicSelectUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("selectClinic"), width = 12),
    htmlOutput(ns("selectClinicCompare"), width = 12)
  )
}

clinicSelectServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      clinics <- data %>%
        distinct(Clinic) %>%
        arrange(Clinic) %>%
        mutate(Clinic = as.character(Clinic))

      selectedClinic <- reactiveVal(NULL)
      selectedClinicCompare <- reactiveVal(NULL)

      observeEvent(input$selectClinicCompare,
        {
          #### Ensure that same value dont trigger rerender
          if (!identical(selectedClinicCompare(), input$selectClinicCompare)) {
            selectedClinicCompare(input$selectClinicCompare)
          }
        },
        ignoreNULL = FALSE
      )

      observeEvent(input$selectClinic, {
        if (!identical(selectedClinic(), input$selectClinic)) {
          selectedClinic(input$selectClinic)
        }
      })

      output$selectClinic <- renderUI({
        pickerInput(ns("selectClinic"),
          "Select Clinic",
          choices = clinics
        )
      })

      output$selectClinicCompare <- renderUI({
        req(input$selectClinic)

        pickerInput(ns("selectClinicCompare"),
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

      return(list(
        selectedClinic,
        selectedClinicCompare
      ))
    }
  )
}
