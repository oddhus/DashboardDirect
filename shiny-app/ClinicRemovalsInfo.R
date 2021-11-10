selectRemovalsFacetRowControl <- function(removalsWithImplants) {
  pickerInput("selectRemovalsFacetRowControl",
              "Select Facet Row",
              choices =
                c(
                  removalsWithImplants %>%
                    select(
                      where(function(col) is.factor(col) | is.logical(col)) &
                        !any_of(c("RefNr"))
                    ) %>%
                    names(),
                  "None"
                ),
              selected = "None"
  )
}

selectSpecificRemovalsFacetRowControl <- function(removalsWithImplants, selectedRow) {
  pickerInput("selectSpecificRemovalsFacetRowControl",
              # Reactive label.
              label = paste0("Select", selectedRow),
              # Reactive choices.
              choices = as.character(sort(unique(removalsWithImplants[[selectedRow]]))),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              )
  )
}

selectRemovalsFactorLevelsControl <- function(removalsWithImplants, selectedFactor) {
  pickerInput("selectRemovalsFactorLevelsControl",
              # Reactive label.
              label = paste0("Select", selectedFactor),
              # Reactive choices.
              choices = as.character(sort(unique(removalsWithImplants[[selectedFactor]]))),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              )
  )
}

selectYAxisRemovalsControl <- function(removalsWithImplants) {
  pickerInput("selectYAxisRemovalsControl",
              "Select Y-axis",
              choices = c(
                removalsWithImplants %>%
                  select(where(function(col) is.numeric(col) | is.logical(col)) &
                           !any_of(c("Id.y", "Id"))) %>%
                  names(),
                "Antall"
              ),
              selected = "Complications"
  )
}

selectXAxisRemovalsControl <- function(removalsWithImplants) {
  pickerInput("selectXAxisRemovalsControl",
              "Select X-axis",
              choices = removalsWithImplants %>%
                select(where(is.factor) &
                         !any_of(c("RefNr"))) %>%
                names(),
              selected = "Clinic"
  )
}

clinicCompareRemovalsPlot <- function(removalsWithImplants,
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
      MeanData <- removalsWithImplants %>%
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
            if_else(is.numeric(removalsWithImplants[[selectedYAxis]]),
                    mean(!!sym(selectedYAxis)),
                    sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
            )
          },
          sd = if (selectedYAxis == "Antall" | !is.numeric(removalsWithImplants[[selectedYAxis]])) {
            NULL
          } else {
            sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n())
          }
        ) %>%
        mutate(Clinic = "Mean")
    }
    
    ClinicData <- removalsWithImplants %>%
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
          if_else(is.numeric(removalsWithImplants[[selectedYAxis]]),
                  mean(!!sym(selectedYAxis)),
                  sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
          )
        },
        sd = if (selectedYAxis == "Antall" | !is.numeric(removalsWithImplants[[selectedYAxis]])) {
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
        if (is.numeric(removalsWithImplants[[selectedYAxis]])) {
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
      ylab(if_else(selectedYAxis == "Antall" | is.numeric(removalsWithImplants[[selectedYAxis]]),
                   selectedYAxis,
                   paste(selectedYAxis, "Percentage", sep = " ")
      ))
  }
}
