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