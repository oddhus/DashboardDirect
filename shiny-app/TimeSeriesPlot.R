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
    ggplot(aes(x = month, y = percentage, color = RemovalReason)) +
    geom_line(size =1.5)+
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
    xlab(paste0("\n", timeScale)) +
    ylab("Percentage\n") +
    theme(
      text = element_text(size=18),
      strip.text = element_text(colour = "grey20"),
    ) +
    {
      if ((isTruthy(implantNames) & length(implantNames) > 1) | (isTruthy(clinics) & length(clinics) > 1)) {
        theme(
          strip.background = element_rect(color = "grey40", size = 1),
          panel.border = element_rect(color = "grey40", fill = NA, size = 0.9)
        )
      }
    } +
    guides(color=guide_legend(title="Removal reason"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
    theme(panel.spacing = unit(2, "lines"))
}



