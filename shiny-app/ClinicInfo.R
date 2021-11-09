library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

selectLabels <- function(col) is.factor(col) | is.logical(col)


complicationsInfo <- function(insertionsWithImplants, selectedClinic) {
  if (isTruthy(selectedClinic)) {
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


selectClinicFacetRowControl <- function(implants) {
  pickerInput("selectClinicFacetRowControl",
    "Select Facet Row",
    choices =
      c(
        implants %>%
          select(
            where(function(col) is.factor(col) | is.logical(col)) &
              !any_of(c("ComplicationsComment", "RefNr", "LotNr", "AntibioticsType"))
          ) %>%
          names(),
        "None"
      ),
    selected = "None"
  )
}

selectSpecificFacetRowControl <- function(implants, selectedRow) {
  pickerInput("selectSpecificFacetRowControl",
    # Reactive label.
    label = if (isTruthy(selectedRow)) paste("Select", selectedRow, "") else "Awaiting input",
    # Reactive choices.
    choices = as.character(sort(unique(implants[[selectedRow]]))),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    )
  )
}

selectCompareAttributeControl <- function(implants, selectedFactor) {
  pickerInput("selectCompareAttributeControl",
    # Reactive label.
    label = if (isTruthy(selectedFactor)) paste("Select", selectedFactor, "") else "Awaiting input",
    # Reactive choices.
    choices = as.character(sort(unique(implants[[selectedFactor]]))),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE,
      size = 10,
      `selected-text-format` = "count > 3"
    )
  )
}

selectYAxisClinicControl <- function(implants) {
  pickerInput("selectYAxisClinicControl",
    "Select Y-axis",
    choices = c(
      implants %>%
        select(where(function(col) is.numeric(col) | is.logical(col))) %>%
        names(),
      "Antall"
    ),
    selected = "Complications"
  )
}

selectXAxisClinicControl <- function(implants) {
  pickerInput("selectXAxisClinicControl",
    "Select X-axis",
    choices = implants %>%
      select(where(is.factor) &
        !any_of(c("ComplicationsComment", "RefNr", "LotNr", "AntibioticsType"))) %>%
      names(),
    selected = "Clinic"
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
                              hideXLab,
                              selectedClinic,
                              compareClinic,
                              selectedFactor,
                              selectSpecificFacetRow,
                              selectLevel,
                              selectedYAxis,
                              selectedXAxis) {
  MeanData <- NULL

  if (!(isTruthy(selectedYAxis) & isTruthy(selectedXAxis) & isTruthy(selectedClinic))) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Select implant")) +
      xlab(NULL)
  } else {
    if (showMean) {
      MeanData <- insertionsWithImplants %>%
        # Filter X-axis to only show selected levels
        filter(
          if (isTruthy(selectLevel)) {
            grepl(
              paste(
                paste("^", selectLevel, "$", sep = ""),
                collapse = "|"
              ),
              # Vector where matches are sought. Need to be dynamic
              !!sym(selectedXAxis)
            )
          } else {
            TRUE
          }
        ) %>%
        # Filter Facets
        filter(
          if (isTruthy(selectSpecificFacetRow)) {
            grepl(
              paste(
                paste("^", selectSpecificFacetRow, "$", sep = ""),
                collapse = "|"
              ),
              # Vector where matches are sought. Need to be dynamic
              !!sym(selectedFactor)
            )
          } else {
            TRUE
          }
        ) %>%
        group_by_at(
          c(
            if (selectedXAxis == "Clinic") NULL else selectedXAxis,
            if (selectedFactor == "None") NULL else selectedFactor
          )
        ) %>%
        summarize(
          value = if (selectedYAxis == "Antall") {
            as.double(n())
          } else {
            if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
              mean(!!sym(selectedYAxis)),
              sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
            )
          },
          sd = if (selectedYAxis == "Antall" | !is.numeric(insertionsWithImplants[[selectedYAxis]])) {
            NULL
          } else {
            sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n())
          }
        ) %>%
        mutate(Clinic = "Mean")
    }

    ClinicData <- insertionsWithImplants %>%
      # Filter to only show selected clinics
      filter(
        selectedClinic == Clinic |
          grepl(
            paste(
              paste("^", compareClinic, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought.
            Clinic
          )
      ) %>%
      # Filter X-axis to only show selected levels
      filter(
        if (is.null(selectLevel)) {
          TRUE
        } else {
          grepl(
            paste(
              paste("^", selectLevel, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            !!sym(selectedXAxis)
          )
        }
      ) %>%
      # Filter facet columns to only show specified columns
      filter(
        if (is.null(selectSpecificFacetRow)) {
          TRUE
        } else {
          grepl(
            paste(
              paste("^", selectSpecificFacetRow, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            !!sym(selectedFactor)
          )
        }
      ) %>%
      group_by_at(
        c(
          "Clinic",
          selectedXAxis,
          if (selectedFactor == "None") NULL else selectedFactor
        )
      ) %>%
      summarise(
        value = if (selectedYAxis == "Antall") {
          as.double(n())
        } else {
          if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
            mean(!!sym(selectedYAxis)),
            sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
          )
        },
        sd = if (selectedYAxis == "Antall" | !is.numeric(insertionsWithImplants[[selectedYAxis]])) {
          NULL
        } else {
          sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n())
        },
      )

    bind_rows(ClinicData, MeanData) %>%
      ggplot(aes(
        x = if (is.null(selectedXAxis)) Clinic else !!sym(selectedXAxis),
        y = value, fill = Clinic
      )) +
      geom_col(position = position_dodge(width = 0.9), width = 0.5) +
      {
        if (is.numeric(insertionsWithImplants[[selectedYAxis]])) {
          geom_errorbar(
            aes(ymin = value - sd, ymax = value + sd),
            position = position_dodge(width = 0.9), width = .25, size = 0.2
          )
        }
      } +
      facet_grid(cols = if (is.null(selectedFactor) | selectedFactor == "None") NULL else vars(!!sym(selectedFactor))) +
      theme(
        axis.text.x = if (hideXLab) element_blank() else element_text(),
        axis.ticks.x = if (hideXLab) element_blank() else element_line()
      ) +
      xlab(if (hideXLab) selectedXAxis else "") +
      ylab(if_else(selectedYAxis == "Antall" | is.numeric(insertionsWithImplants[[selectedYAxis]]),
        selectedYAxis,
        paste(selectedYAxis, "Percentage", sep = " ")
      ))
  }
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
