#CheckFunc

checkFunction <- function(last_updated) {
  if(last_updated() + 3600 < Sys.time()) {
    print("Updated")
    last_updated(Sys.time())
    return("Changed")
  } else {
    return("Unchanged")
  }
}

vectorContainsAnyElement <- function(data,
                                     elements,
                                     searchCol,
                                     vectorNullReturn = TRUE) {
  if (is.null(elements)){
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
