clincToothImplantPlot <- function(data, selectedClinic, overallFilter, overallFilterLevels) {
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels)) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
  filteredData <- data %>%
    mutate(OperationDate = coalesce(RemovalDate, InsertionDate),
           Operation = if_else(is.na(RemovalDate), "Insertion", "Removal"))
  
  filteredData <- filteredData %>%
    arrange(desc(filteredData$OperationDate))  %>%
    filter(InsertionClinic == selectedClinic)
  
  
  filteredData <- filteredData[seq(1,20),] %>%
    group_by(ImplantName, Operation) %>%
    summarise(n = n())
  
  ggbarplot(filteredData, x = "ImplantName", y = "n",
            fill = "Operation",               # change fill color by cyl
            color = "white",            # Set bar border colors to white
            sort.by.groups = FALSE,     # Don't sort inside each group
            x.text.angle = 90,           # Rotate vertically x axis texts
            xlab = "Implant Name",
            ylab = "Number of Operations last\n20 days",
            palette = if("Insertion" %in% filteredData$Operation) c("#00AFBB", "#FC4E07") else c("#FC4E07")
  )
}
