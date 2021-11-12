library(tidyr)

selectLabels <- function(col) is.factor(col) | is.logical(col)


clinicPlot <- function(insertionsWithImplants, selectedClinic, selectedVariables, rangeX) {
  if (is.null(selectedClinic)) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "N/A")) +
      xlab(NULL)
  } else {
    insertionsWithImplants %>%
      # Filter selected clinic
      filter(Clinic == selectedClinic) %>%
      # Group by month
      group_by(month = floor_date(InsertionDate, unit = "month")) %>%
      # Sum all occurrences during the particular months
      summarise(
        insertions = n(),
        complications = sum(Complications, na.rm = TRUE),
        antibioticsUsageBefore = sum(PreOperation, na.rm = TRUE),
      ) %>%
      # Give row number to the data
      mutate(rn = row_number()) %>%
      # Create a longer table, joining columns to one long column
      pivot_longer(cols = c(-rn, -month)) %>%
      # Only show selected factors
      filter(
        case_when(
          # When no input is selected show all
          is.null(selectedVariables) ~ TRUE,
          # Otherwise only selected factors
          T ~ grepl(
            paste(
              paste("^", selectedVariables, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            name
          )
        )
      ) %>%
      ggplot(aes(x = as.Date(month), y = value, color = name)) +
      geom_line() +
      scale_x_date(NULL, date_labels = "%b %y", breaks = scales::breaks_width("1 year")) +
      # Allow dynamic x-axis
      coord_cartesian(xlim = rangeX, expand = FALSE) +
      ylab("Number of Patients")
  }
}
