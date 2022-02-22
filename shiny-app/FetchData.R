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

getCompleteTable <- function() {
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
               !starts_with("InsertionId"))) %>%
    collect() %>%
    mutate(InsertionYear = year(InsertionDate))
    
    
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
           LotNr, -InsertionId) %>%
    collect() %>%
    mutate(InsertionYear = year(InsertionDate))
  
  data <- insertionData %>%
    full_join(removalData , by = c("PatientId", "Position", "InsertionYear")) %>% 
    rename(InsertionClinic = Clinic.x, RemovalClinic = Clinic.y) %>%
    mutate(ImplantLengthMillimeter = coalesce(ImplantLengthMillimeter.x, ImplantLengthMillimeter.y),
           ImplantDiameterMillimeter = coalesce(ImplantDiameterMillimeter.x, ImplantDiameterMillimeter.y),
           Vendor = coalesce(Vendor.x, Vendor.y),
           ImplantName = coalesce(ImplantName.x, ImplantName.y),
           LotNr = coalesce(LotNr.x, LotNr.y),
           InsertionDate = coalesce(InsertionDate.x, InsertionDate.y)) %>%
    select(-(ends_with(".x") | ends_with(".y") | ends_with("Comment") | starts_with("RefNr") | starts_with("IdNumber"))) %>%
    mutate(across(where(is.character) | Position, as.factor)) %>%
    mutate(
      YearsSinceInsertion = time_length(
        interval(
          parse_date_time(InsertionDate, orders = "Ymd HMS", truncated = 3),
          parse_date_time(RemovalDate, orders = "Ymd HMS", truncated = 3)
        ),"years"),
      survt = time_length(
        interval(
          parse_date_time(InsertionDate, orders = "Ymd HMS", truncated = 3),
          parse_date_time(if_else(is.na(RemovalDate), as.POSIXct(today()), RemovalDate), orders = "Ymd HMS", truncated = 3)
        ),"days"),
      survStatus = if_else(is.na(RemovalId), 1, 2),
      survStatusCencored = if_else(survt > 8000, 1, survStatus),
      survTimeCencored = if_else(survt > 8000, 8000, survt)
    ) %>%
    mutate(RemovalBeforeNYear = case_when(
      ceiling(YearsSinceInsertion) == 1 ~ "Year 1",
      ceiling(YearsSinceInsertion) <= 4 ~ "Year 2-4",
      ceiling(YearsSinceInsertion) <= 9 ~ "Year 5-9",
      ceiling(YearsSinceInsertion) >= 10 ~ "Year 10+",
      TRUE ~ as.character(NA)
    ))
  
  data$RemovalBeforeNYear <- factor(data$RemovalBeforeNYear, levels = c("Year 1", "Year 2-4", "Year 5-9", "Year 10+"))
  
  return(data)
}