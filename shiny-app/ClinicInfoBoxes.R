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
  filteredData <- data %>%
    arrange(desc(data$InsertionDate))  %>%
    filter(!is.na(InsertionId) & InsertionClinic == selectedClinic)
    
  
  filteredData <- filteredData[seq(1,20),] %>%
    mutate(events = if_else(is.na(AntibioticsDoseMg) | AntibioticsDoseMg <= 0, 0, 1)) %>%
    group_by(InsertionClinic) %>%
    summarise(cases = n(),
           events = sum(events))
  
  infoBox("Antibiotics usage",
          value = paste((filteredData$events / filteredData$cases) * 100, "%"),
          subtitle = "of the last 20 operations",
          icon = shiny::icon("chart-bar"),
          color = case_when(
            (filteredData$events / filteredData$cases) > 0.85 ~ "green",
            (filteredData$events / filteredData$cases) > 0.6 ~ "yellow",
            TRUE ~ "red"
          ),
          width = 12,
          fill = TRUE)
}

complicationsInfo <- function(data, selectedClinic) {
  filteredData <- data %>%
    arrange(desc(data$InsertionDate))  %>%
    filter(!is.na(InsertionId) & InsertionClinic == selectedClinic)
  
  
  filteredData <- filteredData[seq(1,20),] %>%
    mutate(events = if_else(is.na(Complications) | !Complications, 0, 1)) %>%
    group_by(InsertionClinic) %>%
    summarise(cases = n(),
              events = sum(events))
  
  infoBox("Complications during insertions",
          value = paste((filteredData$events / filteredData$cases) * 100, "%"),
          subtitle = "of the last 20 operations",
          icon = shiny::icon("chart-bar"),
          color = case_when(
            (filteredData$events / filteredData$cases) > 0.85 ~ "green",
            (filteredData$events / filteredData$cases) > 0.6 ~ "yellow",
            TRUE ~ "red"
          ),
          width = 12,
          fill = TRUE)
}

guiderailInfo <- function(data, selectedClinic) {
  filteredData <- data %>%
    arrange(desc(data$InsertionDate))  %>%
    filter(!is.na(InsertionId) & InsertionClinic == selectedClinic)
  
  
  filteredData <- filteredData[seq(1,20),] %>%
    mutate(events = if_else(is.na(GuideRail) | !GuideRail, 0, 1)) %>%
    group_by(InsertionClinic) %>%
    summarise(cases = n(),
              events = sum(events))
  
  infoBox("Guide rail usage",
          value = paste((filteredData$events / filteredData$cases) * 100, "%"),
          subtitle = "of the last 20 operations",
          icon = shiny::icon("chart-bar"),
          color = case_when(
            (filteredData$events / filteredData$cases) > 0.85 ~ "green",
            (filteredData$events / filteredData$cases) > 0.6 ~ "yellow",
            TRUE ~ "red"
          ),
          width = 12,
          fill = TRUE)
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
