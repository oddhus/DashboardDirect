library(dplyr)
library(dbplyr)
library(stringr)

getInsertions <- function(){
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "localhost\\SQLEXPRESS",
    Database = "DentalImplants",
    options(connectionObserver = NULL)
  )
  
  insertion <- tbl(con, "Insertion")
  antibiotics <-  tbl(con, "Antibiotics")
  
  data <- insertion %>% left_join(antibiotics, by = c("AntibioticsId"="Id")) %>% collect()
  
  DBI::dbDisconnect(con)
  
  return(data)
}

getImplants <- function(){
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = "localhost\\SQLEXPRESS",
    Database = "DentalImplants",
    options(connectionObserver = NULL)
  )
  
  implant <- tbl(con, "Implant")
  extractionReason <- tbl(con, "ExtractionReason")
  implantType <- tbl(con, "Implants")
  vendor <- tbl(con, "Vendor")
  material <- tbl(con, "Material")
  
  #First join subtable
  implantType <- implantType %>% left_join(vendor, by = c("VendorId"="Id"))
  
  #Join all tables
  data <- implant %>%
    left_join(implantType, by = c("ImplantsId"="Id")) %>%
    left_join(material, by = c("MaterialId"="Id")) %>%
    left_join(extractionReason, by = c("ExtractionReasonId"="Id")) %>%
    collect()
  
  #Rename
  data <- data %>% 
    rename(ImplantName = Name.x) %>%
    rename(VendorName = Name.y) %>%
    rename(Material = Name.x.x) %>%
    rename(ExtractionReason = Name.y.y) %>%
    mutate(ImplantName = as.factor(iconv(ImplantName,'UTF-8', 'ASCII', sub="byte"))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(Position = as.factor(Position))
  
  DBI::dbDisconnect(con)
  
  return(data)
}



