library(ggplot2)
library(dplyr)
library(shiny)

selectFactorControl <- function(implants) {
  selectInput("selectFactorControl",
    "Select Factor",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select_if(is.factor) %>%
        names()
    ),
    selected = "Position"
  )
}

selectInsertionAttributeControl <- function(implants, selectedFactor) {
  selectInput("selectInsertionAttributeControl",
    # Reactive label. Depends on factor from "select2".
    case_when(
      length(selectedFactor) > 0 ~ paste("Select", selectedFactor, ""),
      T ~ "Awaiting input"
    ),
    # Reactive choices. Depends on factor from "select2".
    choices = case_when(
      is.null(implants) ~ c("Loading"),
      length(selectedFactor) == 0 ~ c("Select factor"),
      T ~ as.character(unique(implants[[selectedFactor]]))
    ),
    multiple = TRUE
  )
}


factorPlot <- function(implants, selectedFactor, selectedInsertionAttribute) {
  if (is.null(implants) | length(selectedFactor) == 0) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "N/A")) +
      xlab(NULL)
  } else {
    implants %>%
      # Filter data based on input values
      filter(
        case_when(
          # When no input is selected show all
          is.null(selectedInsertionAttribute) ~ TRUE,
          # Otherwise only selected factors
          T ~ grepl(
            paste(
              paste("^", selectedInsertionAttribute, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            !!sym(selectedFactor)
          )
        )
      ) %>%
      group_by_at(selectedFactor) %>%
      summarise(percentageComp = sum(Complications, na.rm = TRUE) / n() * 100) %>%
      ggplot(aes_string(x = selectedFactor, y = "percentageComp")) +
      geom_col() +
      ylab("Complication Percentage")
  }
}
