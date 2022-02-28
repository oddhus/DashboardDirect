clinicSuccesRatePlot <- function(data, selectedClinic, overallFilter, overallFilterLevels) {
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels)) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
  allData <- data %>% filter(!is.na(InsertionId)) %>%
  group_by(InsertionClinic) %>%
  summarise(all = n())

  removedData <- data %>% filter(!is.na(InsertionId) &
                             !is.na(RemovalId) &
                             ceiling(survt / 365.5) <= 2) %>%
    group_by(InsertionClinic) %>%
    summarise(removed = n())
  
  combinedData <- allData %>% left_join(removedData, by=c("InsertionClinic")) %>%
    mutate(
      success = all - removed,
      successRate = success / all,
      OverallSuccess = sum(success)/sum(all),
      se = sqrt( OverallSuccess * ((1- OverallSuccess) /all)),
      lcl95 = OverallSuccess   - (1.96* se),
      ucl95 = OverallSuccess   + (1.96* se),
      lcl99.7 = OverallSuccess - (3* se),
      ucl99.7 = OverallSuccess + (3* se),
      )
  
  highlightData <- combinedData %>% filter(InsertionClinic == selectedClinic)
  
  ggscatter(combinedData, x = "all", y = "successRate", 
            color = "black", # Points color, shape and size
            label = "InsertionClinic", label.select = c(selectedClinic),
            xlab = "Number of Insertions",
            ylab = "Implants survival rate after two years"
  ) +
  geom_smooth(aes(y =lcl95),se = FALSE,linetype ="solid",color = "steelblue", size = 1) + 
  geom_smooth(aes(y =ucl95),se = FALSE, linetype ="solid",color = "steelblue", size = 1) +
  geom_smooth(aes(y =lcl99.7),se = FALSE, linetype ="dashed",color = "steelblue", size = 1) +
  geom_smooth(aes(y =ucl99.7),se = FALSE, linetype ="dashed",color = "steelblue", size = 1) +
  geom_hline(aes(yintercept =OverallSuccess), color = "black", linetype="dashed") + 
  geom_point(data=highlightData, aes(x=all,y=successRate), color="coral2", size=4) +
  scale_y_continuous(labels = scales::percent) + 
  font("title", size = 18) +
  font("xlab", size = 14) +
  font("ylab", size = 14) +
  font("xy.text", size = 14)

}
