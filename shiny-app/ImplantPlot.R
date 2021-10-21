library(ggplot2)
library(dplyr)
library(shiny)

implantLengthDiameterPlot <- function(insertionsWithImplants, selectedPosition, LengthOrDiameter) {
  insertionsWithImplants %>%
    filter(Position == selectedPosition) %>%
    mutate(Complications = Complications * 1) %>%
    ggplot(aes_string(
      x = paste("Implant",
        LengthOrDiameter,
        "Millimeter",
        sep = ""
      ),
      y = "Complications",
      col = "Position"
    )) +
    geom_point() +
    geom_jitter(width = 0, height = 0.05) +
    # gghighlight::gghighlight(Position == selectedPosition) +
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    labs(title = paste("Complications Implant ", LengthOrDiameter, sep = "")) +
    theme(legend.position = "none")
  # theme_minimal()
}

implantComplicationPlot <- function(insertionsWithImplants, threshold) {
  insertionsWithImplants %>%
    group_by(ImplantsId) %>%
    summarise(
      percentageComp = sum(Complications, na.rm = TRUE) / n() * 100,
      n = n()
    ) %>%
    ggplot(aes(x = ImplantsId, y = percentageComp)) +
    geom_point(aes(size = n)) +
    gghighlight::gghighlight(
      percentageComp > threshold,
      label_key = ImplantsId,
      use_direct_label = FALSE
    ) +
    labs(
      title = "Complication percentage of Implants",
      y = "Complication Percentage",
      x = "Implant id"
    ) +
    geom_label(aes(label = ImplantsId),
      hjust = 1, vjust = 1, fill = "purple", colour = "white", alpha = 0.5
    )
}

lotNrComplications <- function(insertionsWithImplants, selectedName) {
  if (is.null(selectedName) | selectedName == "None") {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Select implant")) +
      xlab(NULL)
  } else {
    insertionsWithImplants %>%
      filter(ImplantName == selectedName) %>%
      group_by(LotNr) %>%
      summarise(
        percentageComp = sum(Complications, na.rm = TRUE) / n() * 100,
        n = n()
      ) %>%
      ggplot(aes(x = LotNr, y = percentageComp, label = paste("n = ", n, sep = ""))) +
      geom_col() +
      geom_text(vjust = -0.5) +
      labs(
        title = paste(selectedName, " models", sep = ""),
        y = "Complication Percentage",
        x = "LotNr"
      )
  }
}

lotNrFisherTest <- function(insertionsWithImplants, selectedName) {
  if (is.null(selectedName) | is.null(insertionsWithImplants)) {
    "Unable to calculate"
  } else {
    fisher <- insertionsWithImplants %>%
      filter(ImplantName == selectedName) %>%
      group_by(LotNr, Complications) %>%
      summarize(n = n()) %>%
      pivot_wider(names_from = LotNr, values_from = n, values_fill = 0) %>%
      fisher.test()

    strpval <- paste0(
      "p-value: ",
      round(fisher$p.value, 4)
    )
    strconc <- paste0(
      if (fisher$p.value < 0.05) "S" else "No s",
      "ignificant difference in complications between LotNr"
    )

    return(paste(strpval, strconc, sep = "\n"))
  }
}
