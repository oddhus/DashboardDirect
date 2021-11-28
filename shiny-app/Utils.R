vectorContainsAnyElement <- function(data,
                                     elements,
                                     searchCol,
                                     vectorNullReturn = TRUE) {
  if (!(isTruthy(elements) & isTruthy(searchCol)) | isTRUE(searchCol == "None")) {
    return(vectorNullReturn)
  } else {
    grepl(
      paste(
        paste("^", elements, "$", sep = ""),
        collapse = "|"
      ),
      # Vector where matches are sought. Need to be dynamic
      data[[searchCol]]
    )
  }
}
