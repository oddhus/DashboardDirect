explorerPlotSimple <- function(data,
                         x,
                         InsertionsOrRemovals = "Insertions",
                         overallFilter,
                         overallFilterLevels) {
  
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels) & isTruthy(overallFilter != "None")) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
  numericX <- is.numeric(data[[x]])
  logicalX <- is.logical(data[[x]])
  factorX <- is.factor(data[[x]])
  
  if(InsertionsOrRemovals == "Insertions") {
    data <- data %>% filter(!is.na(InsertionId))
  } else {
    data <- data %>% filter(!is.na(RemovalId))
  }
  
  filteredData <- data
  
  if(numericX) {
    return(
      gghistogram(filteredData, x = x,
                  add = "mean", rug = TRUE, fill = "#E7B800")
    )
  }
  
  if(logicalX | factorX) {
   
    filteredData <- filteredData %>%
      group_by_at(x) %>%
      summarise(percentage = n() / nrow(data),
                n = paste0("N = ",as.character(n())))
    
    if ("x" %in% colnames(filteredData)) {
      filteredData <- filteredData %>% rename(!!sym(x) := x)
    }
    
    return(
      ggbarplot(filteredData, x = x, y = "percentage",
                fill = x,
                color = "white",
                x.text.angle = 90,
                label = filteredData$n,
                lab.size = 3,
                ylab = paste0("% of all ", InsertionsOrRemovals)
      ) + 
        scale_y_continuous(labels = scales::percent)
    )
  }
}
