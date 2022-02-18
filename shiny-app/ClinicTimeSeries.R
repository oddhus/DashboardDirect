clinicTimeSeriesPlot <- function(data,
                           selectedClinic,
                           insertionsOrRemovals = "Insertions",
                           predicate,
                           xlab = "nn", ylab = "nn"){
  
  data <- data %>% mutate(year = as.character(
    format(lubridate::floor_date(InsertionDate, "year"),'%Y')))
  
  if (insertionsOrRemovals == "Insertions") {
    data <- data %>% filter(!is.na(InsertionId))
  } else {
    data <- data %>% filter(!is.na(RemovalId))
  }
  
  cases <- data %>%
    group_by(
      year,
      InsertionClinic
    ) %>%
    summarise(cases = n())
  
  events <-  data %>% filter(predicate(.)) %>%
    group_by(
      year,
      InsertionClinic
    ) %>%
    summarise(events = n())
  
  combinedData <- events %>% left_join(cases, by=c("InsertionClinic", "year")) %>%
    filter(!is.na(cases) & !is.na(events) & cases >= 5) %>%
    mutate(
      rate = events / cases,
      OverallRate = sum(events)/sum(cases),
      se = sqrt( OverallRate * ((1- OverallRate) /cases)),
      lcl95 = OverallRate   - (1.96* se),
      ucl95 = OverallRate   + (1.96* se),
      lcl99.7 = OverallRate - (3* se),
      ucl99.7 = OverallRate + (3* se),
    )
  
  ggline(combinedData,
         x = "year", y = "rate", color = "InsertionClinic", size = 1.5,
         xlab = xlab, ylab = ylab) + 
    gghighlight::gghighlight(InsertionClinic == selectedClinic, use_group_by = FALSE,
                             unhighlighted_params = list(size = 0.9))+
  scale_y_continuous(labels = scales::percent) + 
    font("title", size = 18) +
    font("xlab", size = 14) +
    font("ylab", size = 14) +
    font("xy.text", size = 14)
}
