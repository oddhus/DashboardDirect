library(dplyr)
library(dbplyr)
library(stringr)

getEnvs <- function() {
  path <- getwd()
  env <- readLines(paste(path, "/env.txt", sep = ""))
  env.vars <- do.call(rbind, strsplit(sub("=", "\n", env), "\n"))
  env.vars <- as.list(structure(env.vars[, 2], names = env.vars[, 1]))
  return(env.vars)
}

getCon <- function() {
  env <- getEnvs()
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = env$DRIVER, # "SQL Server",
    Server = env$SERVER, # localhost\\SQLEXPRESS
    Database = "DentalImplants", # "DentalImplants",
    Uid = env$UID,
    Pwd = env$PWD,
  )

  return(con)
}

getLocalCon <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "localhost\\SQLEXPRESS",
    Database = "DentalImplants",
  )

  return(con)
}



getInsertions <- function() {
  con <- getLocalCon()

  insertion <- tbl(con, "Insertion")
  antibioticsRelation <- tbl(con, "AntibioticsRelation")
  antibiotics <- tbl(con, "Antibiotics")

  data <- insertion %>%
    left_join(antibioticsRelation, by = c("Id" = "InsertionId")) %>%
    left_join(antibiotics, by = c("AntibioticsId" = "Id")) %>%
    collect()

  DBI::dbDisconnect(con)

  return(data)
}

getImplants <- function() {
  con <- getLocalCon()

  implant <- tbl(con, "Implant")
  extractionReason <- tbl(con, "ExtractionReason")
  implantType <- tbl(con, "Implants")
  vendor <- tbl(con, "Vendor")
  material <- tbl(con, "Material")

  # First join subtable
  implantType <- implantType %>% left_join(vendor, by = c("VendorId" = "Id"))

  # Join all tables
  data <- implant %>%
    left_join(implantType, by = c("ImplantsId" = "Id")) %>%
    left_join(material, by = c("MaterialId" = "Id")) %>%
    left_join(extractionReason, by = c("ExtractionReasonId" = "Id")) %>%
    collect()

  # Rename
  data <- data %>%
    rename(ImplantName = Name.x) %>%
    rename(VendorName = Name.y) %>%
    rename(Material = Name.x.x) %>%
    rename(ExtractionReason = Name.y.y) %>%
    mutate(ImplantName = as.factor(iconv(ImplantName, "UTF-8", "ASCII", sub = "byte"))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Position = as.factor(Position))

  DBI::dbDisconnect(con)

  return(data)
}
