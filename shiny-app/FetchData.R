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
  con <- getCon()

  insertion <- tbl(con, "Insertion")
  antibioticsRelation <- tbl(con, "AntibioticsRelation")
  antibiotics <- tbl(con, "Antibiotics")
  lekholmZarbVolume <- tbl(con, "LekholmZarbVolume")
  lekholmZarbDensity <- tbl(con, "LekholmZarbDensity")
  method <- tbl(con, "Method")
  clinic <- tbl(con, "Clinic")
  

  data <- insertion %>%
    left_join(antibioticsRelation, by = c("Id" = "InsertionId")) %>%
    left_join(antibiotics, by = c("AntibioticsId" = "Id")) %>%
    left_join(lekholmZarbVolume, by = c("LekholmZarbVolumeId" = "Id")) %>%
    left_join(lekholmZarbDensity, by = c("LekholmZarbDensityId" = "Id")) %>%
    left_join(method, by = c("MethodId" = "Id")) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    collect()
  
  # Rename
  data <- data %>%
    rename(AntibioticName = Name.x) %>%
    rename(LekholmZarbVolume = Name.y) %>%
    rename(LekholmZarbDensity = Name.x.x) %>%
    rename(Method = Name.y.y) %>%
    rename(Clinic = Name) %>%
    mutate_if(is.character, as.factor) %>%
    select(-(ends_with("Id") & !starts_with("Id")))

  DBI::dbDisconnect(con)

  return(data)
}

getImplants <- function() {
  con <- getCon()

  implant <- tbl(con, "Implant")
  extractionReason <- tbl(con, "ExtractionReason")
  implantType <- tbl(con, "Implants")
  vendor <- tbl(con, "Vendor")
  material <- tbl(con, "Material")
  stability <- tbl(con, "Stability")
  missingReason <- tbl(con, "MissingReason")
  boneAugmentationMethod <- tbl(con, "BoneAugmentationMethod")
  prostheticConstruction <- tbl(con, "ProstheticConstruction")
  timeUntilLoad <- tbl(con, "TimeOption")
  removal <- tbl(con, "Removal")
  

  # First join subtable
  implantType <- implantType %>% left_join(vendor, by = c("VendorId" = "Id"))

  # Join all tables
  data <- implant %>%
    left_join(implantType, by = c("ImplantsId" = "Id")) %>%
    left_join(material, by = c("MaterialId" = "Id")) %>%
    left_join(extractionReason, by = c("ExtractionReasonId" = "Id")) %>%
    left_join(stability, by = c("StabilityId" = "Id")) %>%
    left_join(missingReason, by = c("MissingReasonId" = "Id")) %>%
    left_join(boneAugmentationMethod, by = c("BoneAugmentationMethodId" = "Id")) %>%
    left_join(prostheticConstruction, by = c("ProstheticConstructionId" = "Id")) %>%
    left_join(timeUntilLoad, by = c("TimeUntilLoadId" = "Id")) %>%
    left_join(removal, by = c("RemovalId" = "Id")) %>%
    collect()

  # Rename
  data <- data %>%
    rename(ImplantName = Name.x) %>%
    rename(VendorName = Name.y) %>%
    rename(Material = Name.x.x) %>%
    rename(ExtractionReason = Name.y.y) %>%
    rename(Stability = Name.x.x.x) %>%
    rename(MissingReason = Name.y.y.y) %>%
    rename(BoneAugmentationMethod = Name.x.x.x.x) %>%
    rename(TimeUntilLoad = Name.y.y.y.y) %>%
    mutate(ImplantName = as.factor(iconv(ImplantName, "UTF-8", "ASCII", sub = "byte"))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Position = as.factor(Position)) %>%
    select(-(ends_with("Id") & !contains("InsertionId")))

  DBI::dbDisconnect(con)

  return(data)
}
