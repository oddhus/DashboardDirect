library(forcats)

implantPlot <- function(removalsWithImplants, removalReason, implantNames, showLotNr) {
  tot <- removalsWithImplants %>%
    group_by(ImplantName) %>%
    summarise(tot = n())

  removalsWithImplants %>%
    filter(
      if (is.null(removalReason)) {
        TRUE
      } else {
        grepl(
          paste(
            paste("^", removalReason, "$", sep = ""),
            collapse = "|"
          ),
          RemovalReason
        )
      }
    ) %>%
    filter(
      if (is.null(implantNames)) {
        TRUE
      } else {
        grepl(
          paste(
            paste("^", implantNames, "$", sep = ""),
            collapse = "|"
          ),
          ImplantName
        )
      }
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
      if(showLotNr) "LotNr" else NULL,
      "RemovalReason"
    )
    ) %>%
    summarise(
      ImplantLengthMillimeter = first(ImplantLengthMillimeter),
      ImplantDiameterMillimeter = first(ImplantDiameterMillimeter),
      n = n()
    ) %>%
    left_join(tot) %>%
    mutate(percentage = n / tot) %>%
    ggplot(aes(x = fct_reorder(ImplantName, percentage),
               y = percentage,
               fill = if(showLotNr) LotNr else NULL)) +
    geom_col() +
    facet_grid(. ~ RemovalReason) +
    coord_flip()
}

