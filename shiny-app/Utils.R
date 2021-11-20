vectorContainsAnyElement <- function(data,
                                     elements,
                                     searchCol,
                                     vectorNullReturn = TRUE) {
  if (is.null(elements)) {
    return(vectorNullReturn)
  }

  grepl(
    paste(
      paste("^", elements, "$", sep = ""),
      collapse = "|"
    ),
    # Vector where matches are sought. Need to be dynamic
    data[[searchCol]]
  )
}
