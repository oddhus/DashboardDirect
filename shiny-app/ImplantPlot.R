library(forcats)

implantPlot <- function(removalsWithImplants, clinics, removalReasons, implantNames, showLotNr) {
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
          if (isTruthy(implantNames)) "ImplantName" else NULL
        ))
      )
    ) %>%
    summarise(tot = n())


  if (isTruthy(removalReasons)) {
    filteredData <- filteredData %>% filter(
      vectorContainsAnyElement(., removalReasons, "RemovalReason")
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
      if (isTruthy(implantNames)) "ImplantName" else NULL,
      if (showLotNr) "LotNr" else NULL,
      "RemovalReason"
    )) %>%
    summarise(
      ImplantLengthMillimeter = first(ImplantLengthMillimeter),
      ImplantDiameterMillimeter = first(ImplantDiameterMillimeter),
      n = n()
    )

  if (isTruthy(clinics) | isTruthy(implantNames)) {
    filteredData <- filteredData %>%
      left_join(tot) %>%
      mutate(percentage = n / tot)
  } else {
    filteredData <- cbind(filteredData, tot) %>%
      mutate(percentage = n / tot)
  }

  filteredData %>%
    ggplot(aes(
      x = fct_reorder(RemovalReason, percentage), # fct_reorder(ImplantName, percentage),
      y = percentage,
      fill = if (showLotNr) LotNr else NULL
    )) +
    geom_col(width = 0.5) +
    {
      if (isTruthy(implantNames) & isTruthy(clinics)) {
        facet_grid(rows = vars(ImplantName), cols = vars(Clinic))
      } else if (isTruthy(implantNames)) {
        facet_grid(rows = vars(ImplantName))
      } else if (isTruthy(clinics)) {
        facet_grid(cols = vars(Clinic))
      }
    } +
    xlab("\nRemoval Reason") +
    {
      if (showLotNr) {
        labs(fill = "LotNr")
      }
    } +
    theme_minimal() +
    theme(
      text = element_text(size=18),
      strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
      strip.text = element_text(colour = "white")
    )
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}
