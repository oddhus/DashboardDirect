data <- getCompleteTable()

allData <- data %>% filter(!is.na(InsertionDate)) %>%
  group_by(ImplantName) %>%
  summarise(all = n())

removed_2yData <- data %>% filter(!is.na(InsertionDate) &
                                 !is.na(RemovalDate) &
                                 ceiling(survt / 365.5) <= 2) %>%
  group_by(ImplantName) %>%
  summarise(removed = n())

removed_5yData <- data %>% filter(!is.na(InsertionDate) &
                                    !is.na(RemovalDate) &
                                    ceiling(survt / 365.5) <= 5) %>%
  group_by(ImplantName) %>%
  summarise(removed = n())

data_2y <- allData %>%
  left_join(removed_2yData, by=c("ImplantName")) %>%
  mutate(
    success = all - removed,
    successRate = success / all,
    se = sqrt( successRate * ((1- successRate) /success)),
    lcl95 = successRate - (1.96* se),
    ucl95 = successRate + (1.96* se)
  )

data_5y <- allData %>%
  left_join(removed_5yData, by=c("ImplantName")) %>%
  mutate(
    success = all - removed,
    successRate = success / all,
    se = sqrt( successRate * ((1- successRate) /success)),
    lcl95 = successRate - (1.96* se),
    ucl95 = successRate + (1.96* se)
  )

data_2y$Year = as.factor(2)
data_5y$Year = as.factor(5)


combinedData <- bind_rows(data_2y, data_5y)

ggdotchart(combinedData, x = "ImplantName", y = "successRate",
           color = "Year",                                # Color by groups
           palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           sorting = "descending",                       # Sort value in descending order
           rotate = TRUE,                                # Rotate vertically
           dot.size = 2,                                 # Large dot size
           ggtheme = theme_pubr(),
           xlab = "Percentage non-removed tooth implants after 2 and 5 years"
) +
  theme_cleveland() + 
  geom_linerange(aes(ymin = lcl95, ymax = ucl95, color = Year))
