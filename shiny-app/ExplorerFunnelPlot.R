explorerFunnelPlot <- function(data,
                                 y,
                                 x,
                                 xLevels = NULL,
                                 factor1 = NULL,
                                 factor2 = NULL,
                                 factorColor = NULL) {
  
  filterFunc <- function(dat, col1) {!is.na(dat[[col1]])}
  
  TrueData <- data %>%
    filter(filterFunc(., x)) %>%
    filter(filterFunc(., y)) %>%
    group_by(
      across(any_of(
        c(
          if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
          if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL,
          if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else NULL,
          x
        )))) %>%
    summarise(
      trueValues = sum(!!sym(y), na.rm = TRUE)
    )
  
  #Must do this for the report functionality to work
  if ("factor1" %in% colnames(TrueData)) {
    TrueData <- TrueData %>% rename(!!sym(factor1) := factor1)
  }
  if ("factor2" %in% colnames(TrueData)) {
    TrueData <- TrueData %>% rename(!!sym(factor2) := factor2)
  }
  if ("factorColor" %in% colnames(TrueData)) {
    TrueData <- TrueData %>% rename(!!sym(factorColor) := factorColor)
  }
  if ("x" %in% colnames(TrueData)) {
    TrueData <- TrueData %>% rename(!!sym(x) := x)
  }
  
  AllData <- data %>%
    filter(filterFunc(., x)) %>%
    filter(filterFunc(., y)) %>%
    group_by(
      across(any_of(
        c(
          if (isTruthy(factor1) & isTRUE(factor1 != "None")) factor1 else NULL,
          if (isTruthy(factor2) & isTRUE(factor2 != "None")) factor2 else NULL,
          if (isTruthy(factorColor) & isTRUE(factorColor != "None")) factorColor else NULL,
          x
        )))) %>%
    summarise(
      allValues = n()
    )
  
  if ("factor1" %in% colnames(AllData)) {
    AllData <- AllData %>% rename(!!sym(factor1) := factor1)
  }
  if ("factor2" %in% colnames(AllData)) {
    AllData <- AllData %>% rename(!!sym(factor2) := factor2)
  }
  if ("factorColor" %in% colnames(AllData)) {
    AllData <- AllData %>% rename(!!sym(factorColor) := factorColor)
  }
  if ("x" %in% colnames(AllData)) {
    AllData <- AllData %>% rename(!!sym(x) := x)
  }
  
  combinedData <- TrueData %>% left_join(AllData, by=c(
    if (isTruthy(factor1) & isTRUE(factor1 != "None")) as.character(factor1) else NULL,
    if (isTruthy(factor2) & isTRUE(factor2 != "None")) as.character(factor2) else NULL,
    if (isTruthy(factorColor) & isTRUE(factorColor != "None")) as.character(factorColor) else NULL,
    as.character(x))) %>%
    mutate(
      successRate = trueValues / allValues,
      OverallSuccess = sum(trueValues)/sum(allValues),
      se = sqrt( OverallSuccess * ((1- OverallSuccess) /allValues)),
      lcl95 = OverallSuccess   - (1.96* se),
      ucl95 = OverallSuccess   + (1.96* se),
      lcl99.7 = OverallSuccess - (3* se),
      ucl99.7 = OverallSuccess + (3* se),
    )
  
  highlightData <- NULL
  if(isTruthy(xLevels)){
    highlightData <- data.frame(combinedData) %>% filter(
      vectorContainsAnyElement(., xLevels, x)
    )
  }
  
  p <- ggscatter(combinedData, x = "allValues", y = "successRate", 
            label = x, repel = TRUE,
            label.select = if(isTruthy(xLevels)) as.vector(highlightData[[x]]) else NULL,
            color = if (isTruthy(factorColor) & isTRUE(factorColor != "None")) as.character(factorColor) else "black",
            xlab = paste0("Number of operations"),
            ylab = paste0(y, " rate %")
  ) +
    geom_smooth(aes(y =lcl95),se = FALSE,linetype ="solid",color = "steelblue", size = 1) + 
    geom_smooth(aes(y =ucl95),se = FALSE, linetype ="solid",color = "steelblue", size = 1) +
    geom_smooth(aes(y =lcl99.7),se = FALSE, linetype ="dashed",color = "steelblue", size = 1) +
    geom_smooth(aes(y =ucl99.7),se = FALSE, linetype ="dashed",color = "steelblue", size = 1) +
    geom_hline(aes(yintercept =OverallSuccess), color = "black", linetype="dashed") + 
    scale_y_continuous(labels = scales::percent) + 
    font("title", size = 18) +
    font("xlab", size = 14) +
    font("ylab", size = 14) +
    font("xy.text", size = 14) + 
    {
      if(isTruthy(xLevels)){
        geom_point(data=highlightData, aes(x=allValues,y=successRate), color="coral2", size=4)
      }
    }
  
  facet(p, 
        facet.by = c(if (isTruthy(factor1) & isTRUE(factor1 != "None")) as.character(factor1) else NULL,
                        if (isTruthy(factor2) & isTRUE(factor2 != "None")) as.character(factor2) else NULL),
        short.panel.labs = FALSE,
        scales = "free_x")
}
