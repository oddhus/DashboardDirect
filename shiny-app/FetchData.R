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

getInsertionsWithImplants <- function() {
  con <- getCon()
  insertions <- getInsertions(con)
  implants <- getImplants(con)

  insertionsWithImplants <- insertions %>%
    left_join(implants, by = c("Id" = "InsertionId")) %>%
    rename(Id = Id.x) %>%
    select(-(ends_with("Id") & !starts_with("Id"))) %>%
    select(-c("Id.y")) %>%
    collect()

  DBI::dbDisconnect(con)

  return(insertionsWithImplants %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(starts_with("Position"), as.factor)))
}

getRemovalsWithImplants <- function() {
  con <- getCon()
  removals <- getRemovals(con)
  implants <- getImplants(con)

  removalsWithImplants <- removals %>%
    left_join(implants, by = c("Id" = "RemovalId")) %>%
    rename(Id = Id.x) %>%
    select(-(ends_with("Id") & !starts_with("Id"))) %>%
    select(-(starts_with("Name"))) %>%
    select(-c(
      "HelfoRefund", "Membrane", "Resorbable", "Boneaugmentation", "Complications", "ComplicationsComment",
      "PositionFrom", "PositionTo", "IsPlateProsthetics", "ExtractionTime", "Position"
    )) %>%
    collect()

  DBI::dbDisconnect(con)

  return(removalsWithImplants %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(YearsSinceInsertion = time_length(
      interval(
        parse_date_time(InsertionDate, orders = "Ymd HMS", truncated = 3),
        parse_date_time(RemovalDate, orders = "Ymd HMS", truncated = 3)
      ),
      "years"
    )))
}

getRemovals <- function(con) {
  removal <- tbl(con, "Removal")
  removalReason <- tbl(con, "RemovalReason")
  clinic <- tbl(con, "Clinic")

  removalData <- removal %>%
    left_join(removalReason, by = c("ReasonId" = "Id")) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    rename(
      RemovalReason = Name.x,
      Clinic = Name.y
    )
}

getInsertions <- function(con) {
  insertion <- tbl(con, "Insertion")
  antibioticsRelation <- tbl(con, "AntibioticsRelation")
  antibiotics <- tbl(con, "Antibiotics")
  lekholmZarbVolume <- tbl(con, "LekholmZarbVolume")
  lekholmZarbDensity <- tbl(con, "LekholmZarbDensity")
  method <- tbl(con, "Method")
  clinic <- tbl(con, "Clinic")


  insertion %>%
    left_join(antibioticsRelation, by = c("Id" = "InsertionId")) %>%
    left_join(antibiotics, by = c("AntibioticsId" = "Id")) %>%
    left_join(lekholmZarbVolume, by = c("LekholmZarbVolumeId" = "Id")) %>%
    left_join(lekholmZarbDensity, by = c("LekholmZarbDensityId" = "Id")) %>%
    left_join(method, by = c("MethodId" = "Id")) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    rename(
      AntibioticsName = Name.x,
      LekholmZarbVolume = Name.y,
      LekholmZarbDensity = Name.x.x,
      Method = Name.y.y,
      Clinic = Name
    )
}

getImplants <- function(con) {
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

  # First join subtable
  implantType <- implantType %>% left_join(vendor, by = c("VendorId" = "Id"))

  # Join all tables
  return(implant %>%
    left_join(implantType, by = c("ImplantsId" = "Id")) %>%
    left_join(material, by = c("MaterialId" = "Id")) %>%
    left_join(extractionReason, by = c("ExtractionReasonId" = "Id")) %>%
    left_join(stability, by = c("StabilityId" = "Id")) %>%
    left_join(missingReason, by = c("MissingReasonId" = "Id")) %>%
    left_join(boneAugmentationMethod, by = c("BoneAugmentationMethodId" = "Id")) %>%
    left_join(prostheticConstruction, by = c("ProstheticConstructionId" = "Id")) %>%
    left_join(timeUntilLoad, by = c("TimeUntilLoadId" = "Id")) %>%
    rename(
      ImplantName = Name.x,
      Brand = Name.y,
      Material = Name.x.x,
      ExtractionReason = Name.y.y,
      Stability = Name.x.x.x,
      MissingReason = Name.y.y.y,
      BoneAugmentationMethod = Name.x.x.x.x,
      TimeUntilLoad = Name.y.y.y.y
    ))
}
