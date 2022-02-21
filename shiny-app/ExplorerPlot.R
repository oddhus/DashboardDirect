explorerPlot <- function(data,
                            y,
                            x,
                            factor1 = NULL,
                            factor1Levels = NULL,
                            factor2 = NULL,
                            factor2Levels = NULL,
                            factorColor = NULL,
                            factorColorLevels = NULL,
                            InsertionsOrRemovals = "Insertions") {
  filteredData <- data
  n_operations <- NULL
  numericX <- is.numeric(data[[x]])
  logicalX <- is.logical(data[[x]])
  factorX <- is.factor(data[[x]])
  numericY <- is.numeric(data[[y]])
  logicalY <- is.logical(data[[y]])
  
  if(InsertionsOrRemovals == "Insertions") {
    filteredData <- filteredData %>% filter(!is.na(InsertionId))
  } else {
    filteredData <- filteredData %>% filter(!is.na(RemovalId))
  }
  
  if (isTruthy(factor1Levels) & isTruthy(factor1) & isFALSE(factor1 == "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., factor1Levels, factor1)
    )
  }
  
  if (isTruthy(factor2Levels) & isTruthy(factor2) & isFALSE(factor2 == "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., factor2Levels, factor2)
    )
  }
  
  if (isTruthy(factorColor) & isTruthy(factorColorLevels) & isFALSE(factorColor == "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., factorColorLevels, factorColor)
    )
  }

  if((logicalY & factorX) | (logicalY & logicalX)){
    filterFunc <- function(dat, col1) {!is.na(dat[[col1]])}
    
    filteredData <- filteredData %>%
      filter(filterFunc(., x)) %>%
      group_by(
        across(any_of(
          c(
            if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
            if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL,
            if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else NULL,
            x
          )))) %>%
      summarise(
        value = sum(!!sym(y), na.rm = TRUE) / nrow(filteredData)
      )
    return(
      ggbarplot(filteredData, x = x, y = "value",
                fill = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else x,
                facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
                             if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL),
                color = "white",            # Set bar border colors to white
                ylab = paste0(y, " percentage of all ", InsertionsOrRemovals)
      )
    )
  }
  
  if(numericY & factorX) {
    return(
      ggviolin(filteredData, x = x, y = y,
               fill = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else x,
               facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
                            if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL),
               add = "boxplot", add.params = list(fill = "white"))
    )
  }
  
  if(numericY & numericX) {
    return(
      ggscatter(filteredData, x = x, y = y,
                shape = 2,
                color = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else "black",
                facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
                             if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL),
                add = "loess", conf.int = TRUE)
    )
  }
}
