survivalPlot <- function(data, factor, levels, additionalFactor, additionalLevels){
  filteredData <- data
  
  if (isTruthy(factor) & isTruthy(levels)) {
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
  
  form <- as.formula(paste0("Surv(survt, survStatus) ~ ", if(isTruthy(factor)) paste0(c(factor,additionalFactor),collapse = "+") else 1))
  
  fit <- do.call(survfit, args = list(formula = form, data = filteredData))
  ggsurvplot(fit, data = filteredData)
}
