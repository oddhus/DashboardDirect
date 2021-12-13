timeSeriesPlot <- function(data, timeScale = "year", clinics, removalReasons, implantNames){
  
  filteredData <- data
  
  if(isTruthy(clinics)){
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., clinics, "Clinic")
    )
  }
  
  if(isTruthy(implantNames)){
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., implantNames, "ImplantName")
    )
  }
    
  total <- filteredData %>%
    group_by(
      month = lubridate::floor_date(RemovalDate, timeScale),
      if (isTruthy(clinics)) !!sym("Clinic") else NULL,
      if (isTruthy(implantNames)) !!sym("ImplantName") else NULL
    ) %>%
    summarise(total = n()) %>%
    rename_with(~paste0("Clinic"), contains("clinics")) %>%
    rename_with(~paste0("ImplantName"), contains("implantNames"))
  
  if(isTruthy(removalReasons)){
    filteredData <- filteredData %>% filter(
        vectorContainsAnyElement(., removalReasons, "RemovalReason")
      )
  }
  
  filteredData <- filteredData %>%
    group_by(
      month = lubridate::floor_date(RemovalDate, timeScale),
      if (isTruthy(clinics)) !!sym("Clinic") else NULL,
      if (isTruthy(implantNames)) !!sym("ImplantName") else NULL,
      RemovalReason) %>%
    summarise(n = n()) %>%
    rename_with(~paste0("Clinic"), contains("clinics")) %>%
    rename_with(~paste0("ImplantName"), contains("implantNames")) %>%
    left_join(total) %>%
    mutate(percentage = n / total)
  
  filteredData %>%
    ggplot(aes(x = month, y = percentage, fill = RemovalReason)) +
    geom_col()+
    {
      if (isTruthy(implantNames) & isTruthy(clinics)) {
        facet_grid(rows = vars(ImplantName), cols = vars(Clinic))
      } else if (isTruthy(implantNames)) {
        facet_grid(rows = vars(ImplantName))
      } else if (isTruthy(clinics)) {
        facet_grid(cols = vars(Clinic))
      }
    }+
    theme_minimal() +
    theme(
      text = element_text(size=18),
      strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
      strip.text = element_text(colour = "white")
    ) +
    xlab(paste0("\n", timeScale))
}



