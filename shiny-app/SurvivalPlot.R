survivalPlot <- function(data, factor, levels, additionalFactor, additionalLevels, time, overallFilter, overallFilterLevels){
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels) & isTruthy(overallFilter != "None")) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
  data <- data %>% mutate(
    survStatusCencored = if_else(survt > time, 1, survStatus),
    survTimeCencored = if_else(survt > time, as.double(time), survt)
  )
  
  filteredData <- data
  
  if (isTruthy(factor) & isTruthy(levels) & isTRUE(factor != "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., levels, factor)
    )
  }
  
  if (isTruthy(additionalFactor) & isTruthy(additionalLevels) & isTRUE(additionalFactor != "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., levels, factor)
    )
  }
  
  if(isTRUE(additionalFactor == "None")){
    additionalFactor <- NULL
  }

  if(isTRUE(factor == "None")){
    factor <- NULL
  }
  
  form <- as.formula(paste0("Surv(survTimeCencored, survStatusCencored) ~ ", if(isTruthy(factor)) paste0(c(factor,additionalFactor),collapse = "+") else 1))
  
  fit <- do.call(survfit, args = list(formula = form, data = filteredData))
  ggsurvplot(fit, data = filteredData)
}
