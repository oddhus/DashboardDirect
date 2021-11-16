library(forcats)

implantPlot <- function(removalsWithImplants, removalReason) {
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
         # Vector where matches are sought. Need to be dynamic
         RemovalReason
       )
     }) %>%
    select(RemovalReason,
            ImplantName,
            ImplantLengthMillimeter,
            ImplantDiameterMillimeter,
            #LotNr,
            Brand) %>%
    group_by(ImplantName,
             #LotNr,
             RemovalReason) %>%
    summarise(ImplantLengthMillimeter = first(ImplantLengthMillimeter),
              ImplantDiameterMillimeter = first(ImplantDiameterMillimeter),
              n = n()) %>%
     left_join(tot) %>%
     mutate(percentage = n / tot) %>%
    ggplot(aes(x = fct_reorder(ImplantName, percentage), y = percentage, fill = RemovalReason)) +
     geom_col()
}