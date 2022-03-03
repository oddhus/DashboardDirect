factorSurvivalPlot <- function(data,
                               factor = NULL,
                               levels = NULL,
                               firstYear = 2,
                               secondYear = 5,
                               overallFilter = NULL,
                               overallFilterLevels = NULL){
  
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels) & isTruthy(overallFilter != "None")) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
  if (isTruthy(factor) & isTruthy(levels) & isTRUE(factor != "None")) {
    data <- data %>% filter(
      vectorContainsAnyElement(., levels, factor)
    )
  }
  
  allData <- data %>% filter(!is.na(InsertionDate)) %>%
    group_by_at(factor) %>%
    summarise(all = n())
  
  removed_FirstYearData <- data %>% filter(ceiling(survt / 365.5) <= firstYear) %>%
    group_by_at(factor) %>%
    summarise(removed = n())
  
  removed_SecondYearData <- data %>% filter(ceiling(survt / 365.5) <= secondYear) %>%
    group_by_at(factor) %>%
    summarise(removed = n())
  
  #Must do this for the report functionality to work
  if ("factor" %in% colnames(removed_FirstYearData)) {
    removed_FirstYearData <- removed_FirstYearData %>% rename(!!sym(factor) := factor)
  }
  if ("factor" %in% colnames(removed_SecondYearData)) {
    removed_SecondYearData <- removed_SecondYearData %>% rename(!!sym(factor) := factor)
  }
  if ("factor" %in% colnames(allData)) {
    allData <- allData %>% rename(!!sym(factor) := factor)
  }
  
  data_FirstYear <- allData %>%
    left_join(removed_FirstYearData, by=c(as.character(factor))) %>%
    mutate(
      success = all - removed,
      successRate = success / all,
      se = sqrt( successRate * ((1- successRate) /success)),
      lcl95 = successRate - (1.96* se),
      ucl95 = successRate + (1.96* se),
      FactorWithN = paste0(!!sym(factor),"(N = " , all, ")")
    )
  
  data_SecondYear <- allData %>%
    left_join(removed_SecondYearData, by=c(as.character(factor))) %>%
    mutate(
      success = all - removed,
      successRate = success / all,
      se = sqrt( successRate * ((1- successRate) /success)),
      lcl95 = successRate - (1.96* se),
      ucl95 = successRate + (1.96* se),
      FactorWithN = paste0(!!sym(factor),"(N = " , all, ")")
    )
  
  data_FirstYear$Year = as.factor(firstYear)
  data_SecondYear$Year = as.factor(secondYear)
  
  combinedData <- bind_rows(data_FirstYear, data_SecondYear)
  
  print(combinedData)
  
  ggdotchart(combinedData, x = "FactorWithN", y = "successRate",
             color = "Year",                                # Color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
             sorting = "descending",                       # Sort value in descending order
             rotate = TRUE,                                # Rotate vertically
             dot.size = 2,                                 # Large dot size
             ggtheme = theme_pubr(),
             ylab = paste0("Percentage of intact tooth implants after ",
                           firstYear, " and ", secondYear, " years"),
             xlab = ""
    ) +
    theme_cleveland() + 
    geom_linerange(aes(ymin = lcl95, ymax = ucl95, color = Year)) +
    font("title", size = 18) +
    font("ylab", size = 14) +
    font("y.text", size = 14) +
    scale_y_continuous(labels = scales::percent, limits = c(NA, 1))
}


