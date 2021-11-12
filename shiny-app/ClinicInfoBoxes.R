complicationsInfo <- function(insertionsWithImplants, selectedClinic) {
  if (isTruthy(selectedClinic)) {
    ClinicInfo <- insertionsWithImplants %>%
      group_by(Clinic, Complications) %>%
      summarise(cnt = n()) %>%
      mutate(freq = round(cnt / sum(cnt), 3)) %>%
      filter(Clinic == selectedClinic & Complications)
    
    return(valueBox(
      paste0(100 * ClinicInfo$freq, "%"),
      "Total complications",
      icon = icon("list"),
      color = "yellow"
    ))
  }
  
  valueBox(
    "--",
    "Total complications",
    icon = icon("list"),
    color = "yellow"
  )
}

insertionsInfo <- function(insertionsWithImplants, selectedClinic) {
  if (isTruthy(selectedClinic)) {
    ClinicInfo <- insertionsWithImplants %>%
      group_by(Clinic) %>%
      summarise(cnt = n()) %>%
      filter(Clinic == selectedClinic)
    
    
    return(valueBox(
      ClinicInfo$cnt,
      "Total Insertions",
      icon = icon("list"),
      color = "purple"
    ))
  }
  
  valueBox(
    "--",
    "Total Insertions",
    icon = icon("list"),
    color = "purple"
  )
}

removalsInfo <- function(removalsWithImplants, selectedClinic) {
  if (isTruthy(selectedClinic)) {
    ClinicInfo <- removalsWithImplants %>%
      group_by(Clinic) %>%
      summarise(cnt = n()) %>%
      filter(Clinic == selectedClinic)
    
    return(valueBox(
      ClinicInfo$cnt,
      "Total removals",
      icon = icon("list"),
      color = "green"
    ))
  }
  
  valueBox(
    "--",
    "Total removals",
    icon = icon("list"),
    color = "green"
  )
}
