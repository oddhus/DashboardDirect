library(forcats)

implantPlot <- function(removalsWithImplants, removalReason, implantNames, showLotNr) {
  if (showLotNr & (is.null(implantNames) | length(implantNames) > 8)) {
    return(
      ggplot() +
        theme_void() +
        geom_text(aes(0, 0, label = "Cannot show LotNr for >8 implants. Select fewer implants.")) +
        xlab(NULL)
    )
  }

  tot <- removalsWithImplants %>%
    group_by(ImplantName) %>%
    summarise(tot = n())

  removalsWithImplants %>%
    filter(
      vectorContainsAnyElement(., removalReason, "RemovalReason")
    ) %>%
    filter(
      vectorContainsAnyElement(., implantNames, "ImplantName")
    ) %>%
    select(
      RemovalReason,
      ImplantName,
      ImplantLengthMillimeter,
      ImplantDiameterMillimeter,
      LotNr,
      Brand
    ) %>%
    group_by_at(c(
      "ImplantName",
      if (showLotNr) "LotNr" else NULL,
      "RemovalReason"
    )) %>%
    summarise(
      ImplantLengthMillimeter = first(ImplantLengthMillimeter),
      ImplantDiameterMillimeter = first(ImplantDiameterMillimeter),
      n = n()
    ) %>%
    left_join(tot) %>%
    mutate(percentage = n / tot) %>%
    ggplot(aes(
      x = fct_reorder(ImplantName, percentage),
      y = percentage,
      fill = if (showLotNr) LotNr else NULL
    )) +
    geom_col(width = 0.5) +
    facet_grid(RemovalReason ~ .) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    xlab("Implant Name") +
    {
      if (showLotNr) {
        labs(fill = "LotNr")
      }
    }
}
