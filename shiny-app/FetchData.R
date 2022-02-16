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

getInsertionsAndRemovals <- function() {
  con <- getLocalCon()
  
  
  insertions <- getInsertions(con)
  removals <- getRemovals(con)
  implants <- getImplants(con)
  
  insertionsWithImplants <- insertions %>%
    left_join(implants, by = c("Id" = "InsertionId")) %>%
    rename(Id = Id.x) %>%
    select(-(ends_with("Id") & !starts_with("Id") & !starts_with("Patient"))) %>%
    select(-c("Id.y"))
  
  removalsWithImplants <- removals %>%
    left_join(implants, by = c("Id" = "RemovalId")) %>%
    rename(Id = Id.x) %>%
    select(-(ends_with("Id") & !starts_with("Id") & !starts_with("Patient"))) %>%
    select(-(starts_with("Name"))) %>%
    select(-c(
      "HelfoRefund", "Membrane", "Resorbable", "Boneaugmentation", "Complications", "ComplicationsComment",
      "PositionFrom", "PositionTo", "IsPlateProsthetics", "ExtractionTime", "Material",
      "ExtractionReason", "Stability", "MissingReason", "BoneAugmentationMethod", "TimeUntilLoad",
      "BoneaugmentationBefore","Id.y", "RefNr"
    ))
  
  completeTable <- insertionsWithImplants %>% inner_join(removalsWithImplants, by = c("PatientId", "Position")) %>% collect()
}
  

getInsertionsWithImplants <- function() {
  con <- getLocalCon()
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

completeTable <- function() {
  con <- getLocalCon()
  implant <- tbl(con, "Implant")
  removal <- tbl(con, "Removal")
  insertion <- tbl(con, "Insertion")
  patients <- tbl(con, "Patient")
  
  extractionReason <- tbl(con, "ExtractionReason")
  material <- tbl(con, "Material")
  stability <- tbl(con, "Stability")
  missingReason <- tbl(con, "MissingReason")
  boneAugmentationMethod <- tbl(con, "BoneAugmentationMethod")
  prostheticConstruction <- tbl(con, "ProstheticConstruction")
  timeUntilLoad <- tbl(con, "TimeOption")
  
  implantData <- implant %>%
    left_join(extractionReason, by = c("ExtractionReasonId" = "Id")) %>%
    rename(ExtractionReason = Name) %>% select(-ExtractionReasonId) %>%
    left_join(material, by = c("MaterialId" = "Id")) %>%
    rename(Material = Name) %>% 
    left_join(stability, by = c("StabilityId" = "Id")) %>%
    rename(Stability = Name) %>% 
    left_join(missingReason, by = c("MissingReasonId" = "Id")) %>%
    rename(MissingReason = Name) %>%
    left_join(boneAugmentationMethod, by = c("BoneAugmentationMethodId" = "Id")) %>%
    rename(BoneAugmentationMethod = Name) %>%
    left_join(prostheticConstruction, by = c("ProstheticConstructionId" = "Id")) %>%
    left_join(timeUntilLoad, by = c("TimeUntilLoadId" = "Id")) %>%
    rename(TimeUntilLoad = Name) 
  
  antibioticsRelation <- tbl(con, "AntibioticsRelation")
  antibiotics <- tbl(con, "Antibiotics")
  antibioticsData <- antibioticsRelation %>%
    left_join(antibiotics, by = c("AntibioticsId" = "Id")) %>%
    rename(AntibioticName = Name) %>% select(-AntibioticsId)
  
  implantType <- tbl(con, "Implants")
  vendor <- tbl(con, "Vendor")
  
  implantTypeData <- implantType %>% rename(ImplantName = Name) %>% 
    left_join(vendor, by =  c("VendorId" = "Id")) %>%
    rename(Vendor = Name) %>% select(-VendorId)
  
  lekholmZarbVolume <- tbl(con, "LekholmZarbVolume")
  lekholmZarbDensity <- tbl(con, "LekholmZarbDensity")
  clinic <- tbl(con, "Clinic")
    
  insertionData <- patients %>%
    inner_join(insertion, by = c("Id" = "PatientId")) %>%
    rename(PatientId = Id.x, InsertionId = Id.y) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    rename(Clinic = Name) %>%
    left_join(lekholmZarbVolume, by = c("LekholmZarbVolumeId" = "Id")) %>%
    rename(LekholmZarbVolume = Name) %>% select(-LekholmZarbVolumeId) %>%
    left_join(lekholmZarbDensity, by = c("LekholmZarbDensityId" = "Id")) %>%
    rename(LekholmZarbDensity = Name) %>% select(-LekholmZarbDensityId) %>%
    left_join(antibioticsData, by = c("InsertionId")) %>%
    inner_join(implant, by=c("InsertionId")) %>%
    left_join(implantTypeData, by = c("ImplantsId" = "Id")) %>%
    select(-(ends_with("Id") &
               !starts_with("PatientId") &
               !starts_with("InsertionId")))
    
    
  removalReason <- tbl(con, "RemovalReason")
    
  removalData <- patients %>% 
    inner_join(removal, by = c("Id" = "PatientId")) %>%
    rename(PatientId = Id.x, RemovalId = Id.y) %>%
    left_join(removalReason, by = c("ReasonId" = "Id")) %>%
    rename(RemovalReason = Name) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    rename(Clinic = Name) %>%
    inner_join(implant, by=c("RemovalId")) %>%
    left_join(implantTypeData, by = c("ImplantsId" = "Id")) %>%
    select(PatientId, Position, RemovalId, InsertionDate, RemovalDate, RemovalReason,
           Clinic, ImplantName, Vendor, Position, ImplantLengthMillimeter, ImplantDiameterMillimeter,
           LotNr, -InsertionId)
  
  data <- insertionData %>%
    full_join(removalData , by = c("PatientId", "Position")) %>% 
    collect()
  
  data <- data %>%
    mutate(ImplantLengthMillimeter = coalesce(ImplantLengthMillimeter.x, ImplantLengthMillimeter.y),
           ImplantDiameterMillimeter = coalesce(ImplantDiameterMillimeter.x, ImplantDiameterMillimeter.y),
           LotNr = coalesce(LotNr.x, LotNr.y),
           InsertionDate = coalesce(InsertionDate.x, InsertionDate.y)) %>%
    select(-(ends_with(".x") | ends_with(".y") | ends_with("Comment") | starts_with("RefNr") | starts_with("IdNumber"))) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(DaysSinceInsertion = time_length(
      interval(
        parse_date_time(InsertionDate, orders = "Ymd HMS", truncated = 3),
        parse_date_time(RemovalDate, orders = "Ymd HMS", truncated = 3)
      ),
      "days"
    ))
  
  
}

getRemovalsWithImplants <- function() {
  con <- getLocalCon()
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
  patients <- tbl(con, "Patient")

  removalData <- removal %>%
    left_join(removalReason, by = c("ReasonId" = "Id")) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    left_join(patients, by = c("PatientId" = "Id")) %>%
    select(-IdNumber) %>%
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
  patients <- tbl(con, "Patient")

  insertion %>%
    left_join(antibioticsRelation, by = c("Id" = "InsertionId")) %>%
    left_join(antibiotics, by = c("AntibioticsId" = "Id")) %>%
    left_join(lekholmZarbVolume, by = c("LekholmZarbVolumeId" = "Id")) %>%
    left_join(lekholmZarbDensity, by = c("LekholmZarbDensityId" = "Id")) %>%
    left_join(method, by = c("MethodId" = "Id")) %>%
    left_join(clinic, by = c("ClinicId" = "Id")) %>%
    left_join(patients, by = c("PatientId" = "Id")) %>%
    select(-IdNumber) %>%
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

getImplantTable <- function(con) {
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
