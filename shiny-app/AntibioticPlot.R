library(ggplot2)
library(dplyr)
library(shiny)

antibioticPlot <- function(insertions, selectedClinics) {
    insertions %>%
      # Filter data based on input values
      filter(
        case_when(
          is.null(selectedClinics) ~ TRUE,
          T ~ grepl(paste(paste("^", selectedClinics, "$", sep = ""), collapse = "|"), ClinicId)
        )
      ) %>%
      ggplot(aes(x = as.factor(ClinicId), y = AntibioticsDoseMg, col = Name)) +
      geom_boxplot() +
      labs(title = "AntibioticsDoseMg", x = "Clinic")
}
