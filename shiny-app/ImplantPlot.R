overviewRemovalReasonPlot <- function(data, removalReasons, years, factor, levels, showMean, overallFilter, overallFilterLevels){
  if(isTruthy(overallFilter) & isTruthy(overallFilterLevels) & isTruthy(overallFilter != "None")) {
    data <- data %>% filter(
      vectorContainsAnyElement(., overallFilterLevels, overallFilter)
    )
  }
  
  showMean <- isTRUE(as.logical(showMean))
  filteredData <- data %>% filter(!is.na(RemovalId))

  meanData <- filteredData %>%
    group_by(RemovalReason) %>%
    summarise(Percentage = n() / nrow(filteredData))
  
  tot <- filteredData %>%
    group_by(across(
      any_of(c(
        "RemovalBeforeNYear",
        if (isTruthy(factor) & isTRUE(factor != "None")) factor else NULL
      ))
    )) %>%
    summarise(tot = n())

  #Must do this or the report generating function will not work
  if ("factor" %in% colnames(tot)) {
    tot <- tot %>% rename(!!sym(factor) := factor)
  }
  
  if (isTruthy(years)) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., years, "RemovalBeforeNYear")
    )
  }
  
  if (isTruthy(removalReasons)) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., removalReasons, "RemovalReason")
    )
    
    meanData <- meanData %>% filter(
      vectorContainsAnyElement(., removalReasons, "RemovalReason")
    )
  }
  
  if ((isTruthy(factor) & isTruthy(levels)) | isTRUE(factor != "None")) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., levels, factor)
    )
  }

  filteredData <- filteredData %>%
    group_by(across(
      any_of(c(
        "RemovalBeforeNYear",
        if (isTruthy(factor) & isTRUE(factor != "None")) factor else NULL,
        "RemovalReason"
      ))
    )) %>%
    summarise(n = n())
  
  #Must do this or the report generating function will not work
  if ("factor" %in% colnames(filteredData)) {
    filteredData <- filteredData %>% rename(!!sym(factor) := factor)
  }
  
  filteredData <- filteredData %>%
    left_join(tot, by = c("RemovalBeforeNYear",
                          if (isTruthy(factor) & isTRUE(factor != "None")) as.character(factor) else NULL)) %>%
    mutate(Percentage = n/tot,
           RemovalBeforeNYear = as.factor(RemovalBeforeNYear),
           nObservations = paste0("N = ", n))
  
  
  p <- ggbarplot(filteredData, x = "RemovalReason", y = "Percentage",
            #fill = "steelblue",
            fill = "RemovalReason",               # change fill color by cyl
            color = "white",            # Set bar border colors to white
            #palette = "aaas",            # jco journal color palett. see ?ggpar
            #sort.val = "asc",           # Sort the value in dscending order
            #sort.by.groups = TRUE,      # Sort inside each group
            #x.text.angle = 90           # Rotate vertically x axis texts,
            xlab = "Removal Reason",
            rotate = TRUE,
            position = position_dodge(0.9),
            label = filteredData$nObservations
  ) + {
    if (showMean) {
      stat_mean(data = meanData,aes(fill = RemovalReason), size = 2, color = "grey20", geom = "point")
    }
  } + 
    labs(fill = "Removal Reason") +
    font("title", size = 18) +
    font("xlab", size = 14) +
    font("ylab", size = 14) +
    font("xy.text", size = 14)+
    scale_y_continuous(labels = scales::percent)
  
  facet(p, 
        facet.by = c("RemovalBeforeNYear",
                     if (isTruthy(factor) & isTRUE(factor != "None")) as.character(factor) else NULL),
        short.panel.labs = TRUE)
}
