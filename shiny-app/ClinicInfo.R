library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

selectLabels <- function(col) is.factor(col) | is.logical(col)


complicationsInfo <- function(insertionsWithImplants, selectedClinic) {
  if (!is.null(selectedClinic)) {
    ClinicInfo <- insertionsWithImplants %>%
      group_by(Clinic, Complications) %>%
      summarise(cnt = n()) %>%
      mutate(freq = round(cnt / sum(cnt), 3)) %>%
      filter(Clinic == selectedClinic & Complications)

    return(valueBox(
      paste0(100 * ClinicInfo$freq, "%"),
      "Complications",
      icon = icon("list"),
      color = "yellow"
    ))
  }

  valueBox(
    "--",
    "Complications",
    icon = icon("list"),
    color = "yellow"
  )
}

insertionsInfo <- function(insertionsWithImplants, selectedClinic) {
  if (!is.null(selectedClinic)) {
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


selectCompareFactorControl <- function(implants) {
  selectInput("selectCompareFactorControl",
    "Select Factor",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select(
          where(function(col) is.factor(col) | is.logical(col)) &
            !any_of(c("ComplicationsComment", "RefNr", "LotNr"))
        ) %>%
        names()
    ),
    selected = "Position"
  )
}

selectCompareAttributeControl <- function(implants, selectedFactor) {
  selectInput("selectCompareAttributeControl",
    # Reactive label.
    case_when(
      length(selectedFactor) > 0 ~ paste("Select", selectedFactor, ""),
      T ~ "Awaiting input"
    ),
    # Reactive choices.
    choices = case_when(
      is.null(implants) ~ c("Loading"),
      length(selectedFactor) == 0 ~ c("Select factor"),
      T ~ as.character(unique(implants[[selectedFactor]]))
    ),
    multiple = TRUE
  )
}

selectYAxisClinicControl <- function(implants) {
  selectInput("selectYAxisClinicControl",
    "Select Y-axis",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select(where(function(col) is.numeric(col) | is.logical(col))) %>%
        names()
    ),
    selected = "Complications"
  )
}

complicationsPlot <- function(insertionsWithImplants, selectedClinic) {
  insertionsWithImplants %>%
    filter(Clinic == selectedClinic) %>%
    # Group by month
    group_by(month = floor_date(InsertionDate, unit = "month")) %>%
    # Sum all occurrences during the particular months
    summarise(
      complicationsPercent = (sum(Complications, na.rm = TRUE) / n()) * 100,
    ) %>%
    ggplot(aes(x = as.Date(month), y = complicationsPercent)) +
    geom_point() +
    geom_smooth() +
    scale_x_date(NULL, date_labels = "%b %y", breaks = scales::breaks_width("1 year")) +
    ylab("Complication Percentage") +
    xlab("Date") +
    theme_minimal()
}


clinicComparePlot <- function(insertionsWithImplants,
                              showMean,
                              showXLab,
                              selectedClinic,
                              compareClinic,
                              selectedFactor,
                              selectedAttribute,
                              selectedYAxis) {
  MeanData <- NULL

  if (showMean) {
    MeanData <- insertionsWithImplants %>%
      filter(
        case_when(
          # When no input is selected show all
          is.null(selectedAttribute) ~ TRUE,
          # Otherwise only selected factors
          T ~ grepl(
            paste(
              paste("^", selectedAttribute, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            !!sym(selectedFactor)
          )
        )
      ) %>%
      group_by_at(
        c(
          selectedFactor
        )
      ) %>%
      summarize(
        value = if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
          mean(!!sym(selectedYAxis)),
          sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
        ),
        sd = if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
          sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n()),
          NULL
        )
      ) %>%
      mutate(Clinic = "Mean")
  }

  ClinicData <- insertionsWithImplants %>%
    filter(
      # Always show selected clinic
      Clinic == selectedClinic |
        case_when(
          # When no input is selected show no other clinics
          is.null(compareClinic) ~ F,
          # Otherwise only selected factors
          T ~ grepl(
            paste(
              paste("^", compareClinic, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought.
            Clinic
          )
        )
    ) %>%
    filter(
      case_when(
        # When no input is selected show all
        is.null(selectedAttribute) ~ TRUE,
        # Otherwise only selected factors
        T ~ grepl(
          paste(
            paste("^", selectedAttribute, "$", sep = ""),
            collapse = "|"
          ),
          # Vector where matches are sought. Need to be dynamic
          !!sym(selectedFactor)
        )
      )
    ) %>%
    group_by_at(
      c(
        "Clinic",
        selectedFactor
      )
    ) %>%
    summarise(
      value = if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
        mean(!!sym(selectedYAxis)),
        sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
      ),
      sd = if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
        sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n()),
        NULL
      ),
    )

  bind_rows(ClinicData, MeanData) %>%
    ggplot(aes(x = Clinic, y = value, fill = Clinic)) +
    geom_col() +
    {
      if (is.numeric(insertionsWithImplants[[selectedYAxis]])) {
        geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = .3)
      }
    } +
    facet_grid(cols = if (is.null(selectedFactor)) NULL else vars(!!sym(selectedFactor))) +
    theme(
      axis.text.x = if (showXLab) element_text() else element_blank(),
      axis.ticks.x = if (showXLab) element_line() else element_blank()
    ) +
    xlab(if (showXLab) "Clinic" else "") +
    ylab(if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
      selectedYAxis,
      paste(selectedYAxis, "Percentage", sep = " ")
    ))
}


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
