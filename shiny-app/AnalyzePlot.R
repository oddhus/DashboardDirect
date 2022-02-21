analyzePlot <- function(data, dependent, numeric, factors, booleans, highlight, type, insertionsOrRemovals = "Insertions") {
  isBinomial <- isTRUE("Binary Logistic Regression" == type)
  isLinear <- isTRUE("Linear Model" == type)

  if (length(numeric) > 1) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Plots with more than one numeric variable are not supported.")) +
      xlab(NULL)
  } else if (length(factors) > 2) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Plots with more than three factors are not supported.")) +
      xlab(NULL)
  } else if (length(booleans) > 2) {
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = "Plots with more than two booleans are not supported.")) +
      xlab(NULL)
  } else {
    if(insertionsOrRemovals == "Insertions") {
      data <- data %>% filter(!is.na(InsertionId))
    } else {
      data <- data %>% filter(!is.na(RemovalId))
    }
    
    data %>%
      mutate_if(is.logical, function(col) as.numeric(col, na.rm = TRUE)) %>%
      ggplot(aes_string(
        x = numeric, y = dependent,
        color = if (isTruthy(factors)) factors[1] else NULL
      )) +
      geom_point() +
      {
        if (isBinomial) {
          geom_jitter(width = 0, height = 0.05)
        }
      } +
      {
        if (isTruthy(highlight) & isTruthy(factors)) {
          gghighlight::gghighlight(
            grepl(
              paste(
                paste("^", highlight, "$", sep = ""),
                collapse = "|"
              ),
              # Vector where matches are sought.
              !!sym(factors[1])
            )
          )
        }
      } +
      {
        if (isBinomial) {
          geom_smooth(method = "glm", na.rm = TRUE, method.args = list(family = "binomial"))
        } else if (isLinear) {
          geom_smooth(method = "lm", na.rm = TRUE)
        }
      } +
      facet_grid(
        rows = if (isTruthy(factors) & length(factors) > 1) vars(!!sym(factors[2])) else NULL,
        cols = if (isTruthy(booleans)) vars(!!sym(booleans[1])) else NULL
      ) +
      labs(title = dependent) +
      theme_minimal() +
      theme(
        text = element_text(size=18),
        strip.background = element_rect(fill = "grey20", color = "grey80", size = 1),
        strip.text = element_text(colour = "white")
      )
  }
}
