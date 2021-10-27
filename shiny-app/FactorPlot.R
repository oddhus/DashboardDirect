library(ggplot2)
library(dplyr)
library(shiny)


optionsToRemove <- c("LotNr", "RefNr")

selectLabels <- function(col) is.factor(col) | is.logical(col)

selectYAxisControl <- function(implants) {
  selectInput("selectYAxisControl",
    "Select Y-axis",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select_if(function(col) is.logical(col) | is.numeric(col)) %>%
        names()
    ),
    selected = "Complications"
  )
}

selectFactorControl <- function(implants) {
  selectInput("selectFactorControl",
    "Select Factor",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select(where(is.factor) & !any_of(c("ComplicationsComment", "RefNr", "AntibioticsType"))) %>%
        names()
    ),
    selected = "Position"
  )
}

selectColorControl <- function(implants) {
  selectInput("selectColorControl",
    "Select Color",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select(where(selectLabels) & !any_of(optionsToRemove)) %>%
        names()
    ),
    selected = NULL,
    multiple = T
  )
}

selectFacetRowControl <- function(implants) {
  selectInput("selectFacetRowControl",
    "Select Facet Row",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select(where(selectLabels) & !any_of(optionsToRemove)) %>%
        names()
    ),
    selected = NULL,
    multiple = T
  )
}

selectFacetColControl <- function(implants) {
  selectInput("selectFacetColControl",
    "Select Facet Col",
    choices = case_when(
      is.null(implants) ~ c("Loading..."),
      T ~ implants %>%
        select(where(selectLabels) & !any_of(optionsToRemove)) %>%
        names()
    ),
    selected = NULL,
    multiple = T
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


factorPlot <- function(implants,
                       selectedYAxis,
                       selectedFactor,
                       selectedInsertionAttribute,
                       selectedColorFactor,
                       selectedFacetRowFactor,
                       selectedFacetColFactor) {
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
      group_by_at(
        c(
          selectedFacetRowFactor,
          selectedFacetColFactor,
          selectedFactor,
          selectedColorFactor
        )
      ) %>%
      summarise(
        n = n(),
        value = if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
          mean(!!sym(selectedYAxis)),
          sum(!!sym(selectedYAxis), na.rm = TRUE) / n() * 100
        ),
        sd = if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
          sd(!!sym(selectedYAxis), na.rm = TRUE) / sqrt(n()),
          NULL
        )
      ) %>%
      ggplot(
        aes_string(
          x = selectedFactor,
          y = "value",
          fill = selectedColorFactor
        )
      ) +
      geom_col() +
      {
        if (is.numeric(insertionsWithImplants[[selectedYAxis]])) {
          geom_errorbar(aes(ymin = value - sd, ymax = value + sd), width = .3)
        }
      } +
      facet_grid(
        rows = if (is.null(selectedFacetRowFactor)) NULL else vars(!!sym(selectedFacetRowFactor)),
        col = if (is.null(selectedFacetColFactor)) NULL else vars(!!sym(selectedFacetColFactor))
      ) +
      geom_text(
        aes(
          y = 0.5,
          fontface = 2,
          label = paste("n = ", n, sep = "")
        ),
        position = position_dodge(0.9),
        angle = 90,
        hjust = "left",
        size = 4
      ) +
      ylab(if_else(is.numeric(insertionsWithImplants[[selectedYAxis]]),
        selectedYAxis,
        paste(selectedYAxis, "Percentage", sep = " ")
      ))
  }
}
