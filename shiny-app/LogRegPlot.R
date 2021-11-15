logRegPlot <- function(data, dependent, independent, additionalFactors, highlight) {
  if(length(independent) > 1 ){
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Plots with more than one numeric variable are not supported.")) +
      xlab(NULL)
  } else if (length(additionalFactors) > 2) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Plots with more than three factors are not supported.")) +
      xlab(NULL)
  } else {
    data %>%
      mutate_at(dependent, function(col) as.numeric(col, na.rm = TRUE)) %>%
      ggplot(aes_string(x = independent, y = dependent,
                        color = if(isTruthy(additionalFactors)) additionalFactors[1] else NULL)) +
      geom_point() +
      geom_jitter(width = 0, height = 0.05) +
      {
        if(isTruthy(highlight) & isTruthy(additionalFactors)){
          gghighlight::gghighlight(
            grepl(
            paste(
              paste("^", highlight, "$", sep = ""),
              collapse = "|"
            ),
            # Vector where matches are sought.
            !!sym(additionalFactors[1])
          ))
        }
      } +
      geom_smooth(method = "glm", na.rm = TRUE, method.args = list(family = "binomial")) +
      facet_grid(rows = if(isTruthy(additionalFactors) & length(additionalFactors > 1)) additionalFactors[2] else NULL) +
      labs(title = dependent) +
      theme(legend.position = "none")
  }
}

