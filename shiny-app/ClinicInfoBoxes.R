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
