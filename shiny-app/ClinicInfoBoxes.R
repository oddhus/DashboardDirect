sumInfo <- function(data, type, color, selectedClinic, allCombined) {
  ClinicInfo <- NULL
  if (allCombined) {
    ClinicInfo <- data %>%
      summarise(cnt = n())
  } else if (isTruthy(selectedClinic)) {
    ClinicInfo <- data %>%
      group_by(Clinic) %>%
      summarise(cnt = n()) %>%
      filter(Clinic == selectedClinic)
  }
  
  return(valueBox(
    if(is.null(ClinicInfo)) "--" else ClinicInfo$cnt,
    paste("Total", type, sep = " "),
    icon = icon("list"),
    color = color
  ))
}

antibioticsInfo <- function(data, selectedClinic) {
  totalData <- data %>%
    filter(!is.na(InsertionId) & Clinic == selectedClinic) %>%
    group_by(Clinic) %>%
    summarise(n = n())
  
  
}

infoBoxesUI <- function(id) {
  ns <- NS(id)
  tagList(
    valueBoxOutput(ns("insertionsBox"), width = 4),
    valueBoxOutput(ns("removalsBox"), width = 4),
    valueBoxOutput(ns("complicationBox"), width = 4),
  )
}


infoBoxesModule <- function(id, insertions, removals, allCombined, selectedClinc) {
  moduleServer(
    id,
    function(input, output, session) {
      output$complicationBox <- renderValueBox({
        sumInfo(
          insertions %>% filter(Complications),
          "Complications", "red", selectedClinc(), allCombined()
        )
      })
      
      output$insertionsBox <- renderValueBox({
        sumInfo(
          insertions,
          "Insertions", "purple", selectedClinc(), allCombined()
        )
      })
      
      output$removalsBox <- renderValueBox({
        sumInfo(
          removals,
          "Removals", "yellow", selectedClinc(), allCombined()
        )
      })
    }
  )
}
