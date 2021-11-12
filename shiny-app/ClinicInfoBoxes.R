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
