# Common ---------------------------------------------------------------------
selectFillColorControl <- function(data) {
  pickerInput("selectFillColorControl",
              "Select Fill Color",
              choices =
                c(
                  data %>%
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

# Removals --------------------------------------------------------------------
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

#Insertions -------------------------------------------------------------------
selectInsertionsFacetRowControl <- function(implants) {
  pickerInput("selectInsertionsFacetRowControl",
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

selectSpecificInsertionsFacetRowControl <- function(implants, selectedRow) {
  pickerInput("selectSpecificInsertionsFacetRowControl",
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

selectInsertionsFactorLevelsControl <- function(implants, selectedFactor) {
  pickerInput("selectInsertionsFactorLevelsControl",
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

selectYAxisInsertionsControl <- function(implants) {
  pickerInput("selectYAxisInsertionsControl",
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

selectXAxisInsertionsControl <- function(implants) {
  pickerInput("selectXAxisInsertionsControl",
              "Select X-axis",
              choices = implants %>%
                select(where(is.factor) &
                         !any_of(c("ComplicationsComment", "RefNr", "LotNr", "AntibioticsType"))) %>%
                names(),
              selected = "Clinic"
  )
}