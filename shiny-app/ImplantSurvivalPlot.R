implantSurvivalPlot <- function(data, selectedImplants, firstYear, secondYear){
  
  if (isTruthy(selectedImplants)) {
    data <- data %>% filter(
      vectorContainsAnyElement(., selectedImplants, "ImplantName")
    )
  }
  
  allData <- data %>% filter(!is.na(InsertionDate)) %>%
    group_by(ImplantName) %>%
    summarise(all = n())
  
  removed_FirstYearData <- data %>% filter(!is.na(InsertionDate) &
                                      !is.na(RemovalDate) &
                                      ceiling(survt / 365.5) <= firstYear) %>%
    group_by(ImplantName) %>%
    summarise(removed = n())
  
  removed_SecondYearData <- data %>% filter(!is.na(InsertionDate) &
                                      !is.na(RemovalDate) &
                                      ceiling(survt / 365.5) <= secondYear) %>%
    group_by(ImplantName) %>%
    summarise(removed = n())
  
  data_FirstYear <- allData %>%
    left_join(removed_FirstYearData, by=c("ImplantName")) %>%
    mutate(
      success = all - removed,
      successRate = success / all,
      se = sqrt( successRate * ((1- successRate) /success)),
      lcl95 = successRate - (1.96* se),
      ucl95 = successRate + (1.96* se),
      ImplanNameWithN = paste0(ImplantName,"(N = " , all, ")")
    )
  
  data_SecondYear <- allData %>%
    left_join(removed_SecondYearData, by=c("ImplantName")) %>%
    mutate(
      success = all - removed,
      successRate = success / all,
      se = sqrt( successRate * ((1- successRate) /success)),
      lcl95 = successRate - (1.96* se),
      ucl95 = successRate + (1.96* se),
      ImplanNameWithN = paste0(ImplantName,"(N = " , all, ")")
    )
  
  data_FirstYear$Year = as.factor(firstYear)
  data_SecondYear$Year = as.factor(secondYear)
  
  combinedData <- bind_rows(data_FirstYear, data_SecondYear)
  
  ggdotchart(combinedData, x = "ImplanNameWithN", y = "successRate",
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
    font("y.text", size = 14)
}


