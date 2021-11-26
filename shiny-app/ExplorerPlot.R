exploreDataPlot <- function(insertionsWithImplants,
                            showMean,
                            hideXLab,
                            selectedClinic,
                            compareClinic,
                            selectedFillColor,
                            selectedFacetRow,
                            selectedSpecificFacetRow,
                            selectedFactorLevels,
                            selectedYAxis,
                            selectedXAxis,
                            combineAll) {
  MeanData <- NULL
  numericXAxis <- is.numeric(insertionsWithImplants[[selectedXAxis]])
  numericYAxis <- is.numeric(insertionsWithImplants[[selectedYAxis]])
  
  if (!(isTruthy(selectedYAxis) & isTruthy(selectedXAxis) & isTruthy(selectedClinic))) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Select implant")) +
      xlab(NULL)
  } else if (selectedYAxis == "Antall" & numericXAxis) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Antall is not supported with a numeric x-axis")) +
      xlab(NULL)
  } else {
    if (showMean & !numericXAxis) {
      MeanData <- insertionsWithImplants %>%
        # Filter Facets
        filter(
          vectorContainsAnyElement(., selectedSpecificFacetRow, selectedFacetRow)
        ) %>%
        group_by_at(
          c(
            if (!isTruthy(selectedFillColor) | identical(selectedFillColor, "None") | !combineAll) NULL else selectedFillColor,
            if (!isTruthy(selectedFacetRow) | identical(selectedFacetRow, "None")) NULL else selectedFacetRow
          )
        ) %>%
        summarize(
          value = if (selectedYAxis == "Antall") {
            n() / nlevels(numericXAxis)
          } else {
            if_else(numericYAxis,
              mean(!!sym(selectedYAxis)),
              sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
            )
          },
          sd = if (selectedYAxis == "Antall" | !numericYAxis) {
            NULL
          } else {
            sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n())
          }
        ) %>%
        mutate(!!sym(selectedXAxis) := "Mean")
    }

    ClinicData <- insertionsWithImplants %>%
      # Filter to only show selected clinics
      filter(
        if (isTRUE(combineAll)) {
          TRUE
        } else {
          selectedClinic == Clinic |
            vectorContainsAnyElement(., compareClinic, "Clinic", FALSE)
        }
      ) %>%
      # Filter X-axis to only show selected levels
      filter(
        # All clinics are select-able when returning to the Clinic tab, even if
        # selecting some of them while in "Combine all".
        if (!isTruthy(combineAll) & selectedXAxis == "Clinic") {
          TRUE
        } else {
          vectorContainsAnyElement(., selectedFactorLevels, selectedXAxis)
        }
      ) %>%
      # Filter facet columns to only show specified columns
      filter(
        vectorContainsAnyElement(., selectedSpecificFacetRow, selectedFacetRow)
      )

    if (!numericXAxis) {
      ClinicData <- ClinicData %>%
        group_by_at(
          c(
            selectedXAxis,
            if (!isTruthy(selectedFillColor) | identical(selectedFillColor,"None")) NULL else selectedFillColor,
            if (!isTruthy(selectedFacetRow) | identical(selectedFacetRow, "None")) NULL else selectedFacetRow
          )
        ) %>%
        summarise(
          value = if (selectedYAxis == "Antall") {
            as.double(n())
          } else {
            if_else(numericYAxis,
              mean(!!sym(selectedYAxis)),
              sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
            )
          },
          sd = if (selectedYAxis == "Antall" | !numericYAxis) {
            NULL
          } else {
            sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n())
          },
        )
    }
    
    # Sometimes it renames the first group_by_at variable to "x"
    if("x" %in% colnames(ClinicData)){
      ClinicData <- ClinicData %>% rename(!!sym(selectedXAxis) := x)
    }

    bind_rows(ClinicData, MeanData) %>%
      ggplot(aes(
        x = if (!isTruthy(selectedXAxis)) Clinic else !!sym(selectedXAxis),
        y = if (numericXAxis) !!sym(selectedYAxis) else value,
        fill = if (!numericXAxis) {
          if (!isTruthy(selectedFillColor) | identical(selectedFillColor, "None")) NULL else !!sym(selectedFillColor)
        },
        color = if (numericXAxis) {
          if (!isTruthy(selectedFillColor) | identical(selectedFillColor == "None")) NULL else !!sym(selectedFillColor)
        }
      )) +
      {
        if (isTruthy(selectedFillColor) & !identical(selectedFillColor, "None")) {
          if (numericXAxis) {
            guides(color = guide_legend(title = selectedFillColor))
          } else {
            guides(fill = guide_legend(title = selectedFillColor))
          }
        }
      } +
      {
        if (numericXAxis) {
          geom_point()
        } else {
          geom_col(position = position_dodge(width = 0.9), width = 0.5)
        }
      } +
      {
        if (numericYAxis & !numericXAxis) {
          geom_errorbar(
            aes(ymin = value - sd, ymax = value + sd),
            position = position_dodge(width = 0.9), width = .25, size = 0.2
          )
        }
      } +
      facet_grid(cols = if (!isTruthy(selectedFacetRow) | identical(selectedFacetRow, "None")) NULL else vars(!!sym(selectedFacetRow))) +
      theme(
        axis.text.x = if (hideXLab) element_blank() else element_text(),
        axis.ticks.x = if (hideXLab) element_blank() else element_line()
      ) +
      xlab(if (hideXLab) "" else selectedXAxis) +
      {
        if (numericXAxis) {
          ylab(selectedYAxis)
        } else {
          ylab(if_else(selectedYAxis == "Antall" | numericYAxis,
            selectedYAxis,
            paste(selectedYAxis, "Percentage", sep = " ")
          ))
        }
      }
  }
}
