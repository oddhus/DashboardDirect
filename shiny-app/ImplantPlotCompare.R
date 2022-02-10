compareImplantsPlot <- function(removalsWithImplants, clinics, removalReason, implantNames, showLotNr, showMean = TRUE) {
  showLotNr <- isTRUE(as.logical(showLotNr))
  
  if (showLotNr & (is.null(implantNames) | length(implantNames) > 8)) {
    return(
      ggplot() +
        theme_void() +
        geom_text(aes(0, 0, label = "Cannot show LotNr for >8 implants. Select fewer implants.")) +
        xlab(NULL)
    )
  }
  
  filteredData <- removalsWithImplants
  
  MeanData <- NULL
  meanColor <- c("Mean" = "red")
  
  if(isTruthy(showMean)){
    totRemovals <- removalsWithImplants %>% summarise(tot = n())
    
    MeanData <- removalsWithImplants %>%
      group_by(RemovalReason) %>% filter(RemovalReason == removalReason) %>%
      summarise(Mean_Percentage = n() / totRemovals$tot)
    }
  
  if (isTruthy(clinics)) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., clinics, "Clinic")
    )
  }
  
  if (isTruthy(implantNames)) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., implantNames, "ImplantName")
    )
  }
  
  tot <- filteredData %>%
    group_by(
      across(
        any_of(c(
          if (isTruthy(clinics)) "Clinic" else NULL,
          "ImplantName"
        ))
      )
    ) %>%
    summarise(tot = n())
  
  print(tot)
  
  
  if (isTruthy(removalReason)) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., removalReason, "RemovalReason")
    )
  }
  
  filteredData <- filteredData %>%
    select(
      Clinic,
      RemovalReason,
      ImplantName,
      ImplantLengthMillimeter,
      ImplantDiameterMillimeter,
      LotNr,
      Brand
    ) %>%
    group_by_at(c(
      if (isTruthy(clinics)) "Clinic" else NULL,
      if (showLotNr) "LotNr" else NULL,
      "RemovalReason",
      "ImplantName"
    )) %>%
    summarise(
      ImplantLengthMillimeter = first(ImplantLengthMillimeter),
      ImplantDiameterMillimeter = first(ImplantDiameterMillimeter),
      n = n()
    )
  
  

  filteredData <- filteredData %>%
      left_join(tot) %>%
      mutate(Percentage = n / tot)
  
  
  if (isTruthy(showMean)){
    filteredData <- filteredData %>%
      left_join(MeanData)
  }
  
  filteredData$RemovalReason <- str_wrap(filteredData$RemovalReason, width = 30)
  
  filteredData %>%
    ggplot(aes(
      x = fct_reorder(ImplantName, Percentage),
      y = Percentage,
      fill = if (showLotNr) LotNr else NULL
    )) +
    geom_col(width = 0.5) +
    {
      if (isTruthy(clinics)) {
        facet_grid(cols = vars(Clinic))
      }
    } + {
      if (showMean){
        geom_hline(aes(yintercept = MeanData$Mean_Percentage, color = "Mean"), size = 1.5)
      }
    } +
    xlab("\nImplant Name") +
    {
      if (showLotNr) {
        labs(fill = "LotNr")
      }
    } +
    ylab(paste0(removalReason, " Percentage\n" )) +
    theme_minimal() +
    theme(
      text = element_text(size=18),
      strip.text = element_text(colour = "grey20"),
    ) +
    {
      if (isTruthy(clinics) & length(clinics) > 1) {
        theme(
          strip.background = element_rect(color = "grey40", size = 1),
          panel.border = element_rect(color = "grey40", fill = NA, size = 1)
        )
      }
    } +
    theme(panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0))+
    {
      if (isTruthy(showMean)){
        labs(color = "")
      }
    } +  {
      if (isTruthy(showMean)){
        scale_color_manual(values = meanColor)
      }
    } 
  # {
  #     if (!isTruthy(removalReason) | isTruthy(implantNames) | isTruthy(clinics)) {
  #       theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  #     }
  #   }
}
