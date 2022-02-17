clinicSuccesRatePlot <- function(data) {
  allData <- data %>% filter(!is.na(InsertionId)) %>%
  group_by(Clinic) %>%
  summarise(all = n())

  removedData <- data %>% filter(!is.na(InsertionId) &
                             !is.na(RemovalId) &
                             ceiling(survt / 365.5) <= 2) %>%
    group_by(Clinic) %>%
    summarise(removed = n())
  
  combinedData <- allData %>% left_join(removedData, by=c("Clinic")) %>%
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
  
  ggscatter(combinedData, x = "all", y = "successRate", 
            color = "black", # Points color, shape and size
            label = "Clinic", repel = TRUE,
            xlab = "Number of Insertions",
            ylab = "Implants survival rate after two years %"
  ) +
  geom_smooth(aes(y =lcl95),se = FALSE,linetype ="solid",color = "steelblue", size = 1) + 
  geom_smooth(aes(y =ucl95),se = FALSE, linetype ="solid",color = "steelblue", size = 1) +
  geom_smooth(aes(y =lcl99.7),se = FALSE, linetype ="dashed",color = "steelblue", size = 1) +
  geom_smooth(aes(y =ucl99.7),se = FALSE, linetype ="dashed",color = "steelblue", size = 1) +
  geom_hline(aes(yintercept =OverallSuccess), color = "black", linetype="dashed") + 
  scale_y_continuous(labels = scales::percent)

}
