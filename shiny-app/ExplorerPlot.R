exploreDataPlot <- function(data,
                            showMean,
                            hideXLab,
                            selectedClinic,
                            compareClinic,
                            fillColor,
                            facetRow,
                            specificFacetRow,
                            factorLevels,
                            selectedYAxis,
                            selectedXAxis,
                            combineAll) {
  MeanData <- NULL
  numericXAxis <- is.numeric(data[[selectedXAxis]])
  numericYAxis <- is.numeric(data[[selectedYAxis]])

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
      MeanData <- data
      # Filter Facets
      if (!(isTruthy(specificFacetRow) & isTruthy(facetRow)) | isTRUE(facetRow == "None")) {
        MeanData <- MeanData %>% filter(
          vectorContainsAnyElement(., specificFacetRow, facetRow)
        )
      }

      MeanData %>%
        group_by(across(any_of(
          c(
            if (isTruthy(fillColor) & isFALSE(fillColor == "None") & combineAll) fillColor else NULL,
            if (isTruthy(facetRow) & isFALSE(facetRow == "None")) facetRow else NULL
          )
        ))) %>%
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

    ClinicData <- data %>%
      # Filter to only show selected clinics
      filter(
        if (isTRUE(as.logical(combineAll))) {
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
        if (isFALSE(as.logical(combineAll)) & selectedXAxis == "Clinic") {
          TRUE
        } else {
          vectorContainsAnyElement(., factorLevels, selectedXAxis)
        }
      )

    # Filter facet rows if facet row and specific facet row is present
    if (!(isTruthy(specificFacetRow) & isTruthy(facetRow)) | isTRUE(facetRow == "None")) {
      ClinicData <- ClinicData %>% filter(
        vectorContainsAnyElement(., specificFacetRow, facetRow)
      )
    }

    if (!numericXAxis) {
      ClinicData <- ClinicData %>%
        group_by(across(any_of(
          c(
            selectedXAxis,
            if (isTruthy(fillColor) & isFALSE(fillColor == "None")) fillColor else NULL,
            if (isTruthy(facetRow) & isFALSE(facetRow == "None")) facetRow else NULL
          )
        ))) %>%
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

    if ("x" %in% colnames(ClinicData)) {
      ClinicData <- ClinicData %>% rename(!!sym(selectedXAxis) := x)
    }
    
    if ("factorLevels" %in% colnames(ClinicData)) {
      ClinicData <- ClinicData %>% rename(!!sym(factorLevels) := factorLevels)
    }
    
    
    if ("fillColor" %in% colnames(ClinicData)) {
      ClinicData <- ClinicData %>% rename(!!sym(fillColor) := fillColor)
    }
    
    if ("facetRow" %in% colnames(ClinicData)) {
      ClinicData <- ClinicData %>% rename(!!sym(facetRow) := facetRow)
    }
    
    if ("specificFacetRow" %in% colnames(ClinicData)) {
      ClinicData <- ClinicData %>% rename(!!sym(specificFacetRow) := specificFacetRow)
    }
    
    

    bind_rows(ClinicData, MeanData) %>%
      ggplot(aes(
        x = if (!isTruthy(selectedXAxis)) Clinic else !!sym(selectedXAxis),
        y = if (numericXAxis) !!sym(selectedYAxis) else value,
        fill = if (!numericXAxis) {
          if (!isTruthy(fillColor) | isTRUE(fillColor == "None")) NULL else !!sym(fillColor)
        },
        color = if (numericXAxis) {
          if (!isTruthy(fillColor) | isTRUE(fillColor == "None")) NULL else !!sym(fillColor)
        }
      )) +
      {
        if (isTruthy(fillColor) & isFALSE(fillColor == "None")) {
          if (numericXAxis) {
            guides(color = guide_legend(title = fillColor))
          } else {
            guides(fill = guide_legend(title = fillColor))
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
      {
        if (isTruthy(facetRow) & isFALSE(facetRow == "None")) {
          facet_grid(cols = vars(!!sym(facetRow)))
        }
      } +
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
