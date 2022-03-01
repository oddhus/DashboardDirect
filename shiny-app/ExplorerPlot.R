source("shiny-app/ExplorerFunnelPlot.R")

explorerPlot <- function(data,
                            y,
                            x,
                            xLevels = NULL,
                            factor1 = NULL,
                            factor1Levels = NULL,
                            factor2 = NULL,
                            factor2Levels = NULL,
                            factorColor = NULL,
                            factorColorLevels = NULL,
                            InsertionsOrRemovals = "Insertions",
                            overallFilter = NULL,
                            overallFilterLevels = NULL) {
  
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels) & isTruthy(overallFilter != "None")) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
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
    return(
      explorerFunnelPlot(data = filteredData,
                         y = y,
                         x = x,
                         xLevels = xLevels,
                         factor1 = factor1,
                         factor2 = factor2,
                         factorColor = factorColor)
    )
  }
  
  #Placed after explorer funnel plot because that function takes care of the filtering.
  if (isTruthy(x) & isTruthy(xLevels) & isFALSE(xLevels == "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., xLevels, x)
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
                shape = 1,
                color = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else "black",
                facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
                             if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL),
                add = "loess", conf.int = TRUE)
    )
  }
  
  if(logicalY & numericX) {
    return(
      ggboxplot(filteredData, x = y, y = x,
                fill = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else y,
                facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
                             if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL),
                orientation = "horizontal")
    )
  }
  
  if (logicalX & numericY) {
    return(
      ggboxplot(filteredData, x = x, y = y,
                fill = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else x,
                facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
                             if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL))
    )
  }
}
