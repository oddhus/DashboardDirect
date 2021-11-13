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
  
  if (!(isTruthy(selectedYAxis) & isTruthy(selectedXAxis) & isTruthy(selectedClinic))) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Select implant")) +
      xlab(NULL)
  } else {
    if (showMean) {
      MeanData <- insertionsWithImplants %>%
        # Filter Facets
        filter(
          if (isTruthy(selectedSpecificFacetRow)) {
            grepl(
              paste(
                paste("^", selectedSpecificFacetRow, "$", sep = ""),
                collapse = "|"
              ),
              # Vector where matches are sought. Need to be dynamic
              !!sym(selectedFacetRow)
            )
          } else {
            TRUE
          }
        ) %>%
        group_by_at(
          c(
            if (selectedFillColor == "None") NULL else selectedFillColor,
            if (selectedFacetRow == "None" ) NULL else selectedFacetRow
          )
        ) %>%
        summarize(
          value = if (selectedYAxis == "Antall") {
            n() / nlevels(insertionsWithImplants[[selectedXAxis]])
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
        mutate(!!sym(selectedXAxis) := "Mean")
    }
    
    print(MeanData)
    
    ClinicData <- insertionsWithImplants %>%
      # Filter to only show selected clinics
      filter(
        if(isTruthy(combineAll)){
          TRUE
        }else {
          selectedClinic == Clinic |
            grepl(
              paste(
                paste("^", compareClinic, "$", sep = ""),
                collapse = "|"
              ),
              # Vector where matches are sought.
              Clinic
            ) }
      ) %>%
      # Filter X-axis to only show selected levels
      filter(
        if (is.null(selectedFactorLevels)) {
          TRUE
        } else {
          grepl(
            paste(
              paste("^", selectedFactorLevels, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            !!sym(selectedXAxis)
          )
        }
      ) %>%
      # Filter facet columns to only show specified columns
      filter(
        if (is.null(selectedSpecificFacetRow)) {
          TRUE
        } else {
          grepl(
            paste(
              paste("^", selectedSpecificFacetRow, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought. Need to be dynamic
            !!sym(selectedFacetRow)
          )
        }
      ) %>%
      group_by_at(
        c(
          selectedXAxis,
          if (!isTruthy(selectedFillColor) | selectedFillColor == "None" ) NULL else selectedFillColor,
          if (selectedFacetRow == "None") NULL else selectedFacetRow
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
        x = if (!isTruthy(selectedXAxis)) Clinic else !!sym(selectedXAxis),
        y = value,
        fill = if(!isTruthy(selectedFillColor) | selectedFillColor == "None") NULL else !!sym(selectedFillColor)
      )) + 
      {
        if(isTruthy(selectedFillColor) & selectedFillColor != "None"){
          guides(fill = guide_legend(title = selectedFillColor))
        }
      } +
      geom_col(position = position_dodge(width = 0.9), width = 0.5) +
      {
        if (is.numeric(insertionsWithImplants[[selectedYAxis]])) {
          geom_errorbar(
            aes(ymin = value - sd, ymax = value + sd),
            position = position_dodge(width = 0.9), width = .25, size = 0.2
          )
        }
      } +
      facet_grid(cols = if (is.null(selectedFacetRow) | selectedFacetRow == "None") NULL else vars(!!sym(selectedFacetRow))) +
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

