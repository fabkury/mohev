## The State and Trends of PDA, Smartphone and Tablet Vendor Use in US Hospitals
# --
# By Raymonde Uy, MD, MBA, and Fabricio Kury, MD, Paul Fontelo, MD, MPH
#
# Codename: MOHEV -- Mobile Health Vendors
# Project start: May 2015
# 

#
## Globals
data_dir <- "../../../Data/"
output_dir <- "Output/"

load.libraries <- function(libraries_needed) {
  for(library_needed in libraries_needed)
    if(!library(library_needed, quietly=TRUE, logical.return=TRUE, character.only=TRUE)) {
      install.packages(library_needed)
      if(!library(library_needed, quietly=TRUE, logical.return=TRUE, character.only=TRUE))
        stop(paste("Unable to load library '", library_needed, "'.", sep=""))
    }
}

guarantee.removal <- function(file) {
  if(file.exists(file))
    file.remove(file)
  if(file.exists(file))
    stop(paste0("Unable to erase file '", file, "'."))
}

makeHospitalsPerTypeVendor <- function(year) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, paste0("select a.EquipmentType, Vendor as VendorName, round(N*100/TotalN, 1) as P", year,
" from
(SELECT switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
  'Tablet', true, Type) as EquipmentType,
  switch(VendorName LIKE '%HITACHI%', 'HITACHI LTD',
    VendorName LIKE '%SAMSUNG%', 'SAMSUNG',
    VendorName LIKE '%DELL%', 'DELL',
    VendorName LIKE '%IN MOTION%', 'BlackBerry Limited',
    VendorName LIKE '%In Motion%', 'BlackBerry Limited',
    VendorName LIKE '%Berry%', 'BlackBerry Limited',
    VendorName Like '%ACER%', 'ACER AMERICA CORP.',
    VendorName Like '%Acer%', 'ACER AMERICA CORP.',
    VendorName Like '%HEWLETT%', 'HEWLETT-PACKARD CO.',
    VendorName Like '%SYMBOL%', 'MOTOROLA, INC.',
    VendorName Like '%MOTOROLA%', 'MOTOROLA, INC.',
    VendorName Like '%Panasonic%', 'PANASONIC',
    true, VendorName) as Vendor, count(HAEntityId) as N
from WirelessVendor
group by
  switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
    'Tablet', true, Type),
  switch(VendorName LIKE '%HITACHI%', 'HITACHI LTD',
    VendorName LIKE '%SAMSUNG%', 'SAMSUNG',
    VendorName LIKE '%DELL%', 'DELL',
    VendorName LIKE '%IN MOTION%', 'BlackBerry Limited',
    VendorName LIKE '%In Motion%', 'BlackBerry Limited',
    VendorName LIKE '%Berry%', 'BlackBerry Limited',
    VendorName Like '%ACER%', 'ACER AMERICA CORP.',
    VendorName Like '%Acer%', 'ACER AMERICA CORP.',
    VendorName Like '%HEWLETT%', 'HEWLETT-PACKARD CO.',
    VendorName Like '%SYMBOL%', 'MOTOROLA, INC.',
    VendorName Like '%MOTOROLA%', 'MOTOROLA, INC.',
    VendorName Like '%Panasonic%', 'PANASONIC',
    true, VendorName)) a,
(SELECT EquipmentType, count(*) as TotalN
from (select distinct switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
  'Tablet', true, Type) as EquipmentType, HAEntityId from WirelessVendor)
group by EquipmentType) b
where a.EquipmentType=b.EquipmentType
  and a.EquipmentType not like '%arcode%'
  and a.EquipmentType not like '%WOW%'
  and a.EquipmentType not like '%Cart%'
  and a.EquipmentType not like '%VoIP%'
  and a.EquipmentType not like '%WLAN%'
  and a.EquipmentType not like '%aptop%'"))
  close(mdbconn)  
  retval
}

makeHospitalsPerVendor <- function(year) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, paste0("select Vendor as VendorName, round(N*100/TotalN, 2) as P", year,
" from
(SELECT Vendor, count(*) as N
  from (select distinct switch(VendorName LIKE '%HITACHI%', 'HITACHI LTD',
    VendorName LIKE '%SAMSUNG%', 'SAMSUNG',
    VendorName LIKE '%DELL%', 'DELL',
    VendorName LIKE '%IN MOTION%', 'BlackBerry Limited',
    VendorName LIKE '%In Motion%', 'BlackBerry Limited',
    VendorName LIKE '%Berry%', 'BlackBerry Limited',
    VendorName Like '%ACER%', 'ACER AMERICA CORP.',
    VendorName Like '%Acer%', 'ACER AMERICA CORP.',
    VendorName Like '%HEWLETT%', 'HEWLETT-PACKARD CO.',
    VendorName Like '%SYMBOL%', 'MOTOROLA, INC.',
    VendorName Like '%MOTOROLA%', 'MOTOROLA, INC.',
    VendorName Like '%Panasonic%', 'PANASONIC',
    true, VendorName) as Vendor, HAEntityId from WirelessVendor
      where Type not like '%arcode%'
        and Type not like '%WOW%'
        and Type not like '%Cart%'
        and Type not like '%VoIP%'
        and Type not like '%WLAN%'
        and Type not like '%aptop%')
  group by Vendor) a,
(SELECT count(*) as TotalN
  from (select distinct HAEntityId from WirelessVendor
      where Type not like '%arcode%'
        and Type not like '%WOW%'
        and Type not like '%Cart%'
        and Type not like '%VoIP%'
        and Type not like '%WLAN%'
        and Type not like '%aptop%')) b"))
  close(mdbconn)  
  retval
}


makeSpecificNVendors <- function(year, n) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, paste0("SELECT a.EquipmentType, round(N*100/TotalN, 2) as P", year,
" from (select switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
      'Tablet', true, Type) as EquipmentType, count(*) as N
    from (select Type, HAEntityId, count(*) as NHospitals from WirelessVendor
        group by Type, HAEntityId having count(*) = ", n, ")
    where Type not like '%arcode%' and Type not like '%WOW%' and Type not like '%Cart%'
      and Type not like '%VoIP%' and Type not like '%WLAN%' and Type not like '%aptop%'
    group by switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
      'Tablet', true, Type)) as a
inner join (select switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
    'Tablet', true, Type) as EquipmentType, count(*) as TotalN from WirelessVendor
  group by switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%',
    'Tablet', true, Type)) as b
  on a.EquipmentType=b.EquipmentType;"))
  close(mdbconn)  
  retval
}


makeVendorsPerType <- function(year) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, paste0("select EquipmentType, count(*) as N", year,
" from (select distinct * from (select
  switch(Type LIKE '%ablet%lanned%', 'Tablet Planned', Type LIKE '%ablet%', 'Tablet', true, Type) as EquipmentType,
  switch(VendorName LIKE '%HITACHI%', 'HITACHI LTD',
    VendorName LIKE '%SAMSUNG%', 'SAMSUNG',
    VendorName LIKE '%DELL%', 'DELL',
    VendorName LIKE '%IN MOTION%', 'BlackBerry Limited',
    VendorName LIKE '%In Motion%', 'BlackBerry Limited',
    VendorName LIKE '%Berry%', 'BlackBerry Limited',
    VendorName Like '%ACER%', 'ACER AMERICA CORP.',
    VendorName Like '%Acer%', 'ACER AMERICA CORP.',
    VendorName Like '%HEWLETT%', 'HEWLETT-PACKARD CO.',
    VendorName Like '%SYMBOL%', 'MOTOROLA, INC.',
    VendorName Like '%MOTOROLA%', 'MOTOROLA, INC.',
    VendorName Like '%Panasonic%', 'PANASONIC',
    true, VendorName) as Vendor from WirelessVendor
      where Type not like '%arcode%'
        and Type not like '%WOW%'
        and Type not like '%Cart%'
        and Type not like '%VoIP%'
        and Type not like '%WLAN%'
        and Type not like '%aptop%'))
  group by EquipmentType;"))
  close(mdbconn)  
  retval
}

makeFigure2Respondents <- function(year_list) {
  for(year in year_list) {
    mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
    retval <- sqlQuery(mdbconn, "select count(*) as TotalN from
      (select distinct HAEntityId from WirelessVendor where Type='PDA');")
    close(mdbconn)  
    print(paste0(year, ': ', round(retval[[1]] * 100/ getNumberOfHospitals(year), 2)))
  }
}

makeFigure3Respondents <- function(year) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, "select count(*) as TotalN from
    (select distinct HAEntityId from WirelessVendor where Type LIKE '%martphone%');")
  close(mdbconn)  
  retval[[1]] * 100 / getNumberOfHospitals(year)
}

makeFigure4Respondents <- function(year) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, "select count(*) as TotalN from
    (select distinct HAEntityId from WirelessVendor where Type like '%ablet%' and Type not like '%lanned%');")
  close(mdbconn)  
  retval[[1]] * 100 / getNumberOfHospitals(year)
}

getNumberOfHospitals <- function(year) {
  mdbconn <- odbcConnectAccess(paste0(data_dir, 'HADB ', year, '.mdb'))
  retval <- sqlQuery(mdbconn, "select count(*) as NHospitals from
    (select distinct HAEntityId from HAEntity where HAEntityType = 'Hospital');")
  close(mdbconn)  
  retval[[1]]
}

#
## Execution starts here.

message("The State and Trends of PDA, Smartphone and Tablet Vendor Use in US Hospitals")
message("By Raymonde Uy, MD, MBA, and Fabricio Kury, MD, Paul Fontelo, MD, MPH")
message("Codename: \"MOHEV\" - \"Mobile Health Vendors\"")
message("Code and results available at: http://github.com/fabkury/mohev")
message("Project start: May 2015")

message("Dependencies:")
message(" . HIMSS Analytics Database in MDB files.")
message(" . Write permission on destination directory.")
message(" . R libraries RODBC and xlsx.")

message("Loading libraries...")
load.libraries(c('RODBC', 'xlsx'))

year_list <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012)

message(paste0("Computing years ", paste(year_list, collapse=', '), '.'))

res <- makeHospitalsPerTypeVendor(year_list[1])
for(year in year_list[-1])
  res = merge(res, makeHospitalsPerTypeVendor(year), by=c('EquipmentType', 'VendorName'), all = TRUE)
write.xlsx(res, paste0(output_dir, 'HospitalsPerTypeVendor.xlsx'), row.names=FALSE, showNA=FALSE)

res <- makeHospitalsPerVendor(year_list[1])
for(year in year_list[-1])
  res = merge(res, makeHospitalsPerVendor(year), by='VendorName', all = TRUE)
write.xlsx(res, paste0(output_dir, 'HospitalsPerVendor.xlsx'), row.names=FALSE, showNA=FALSE)

res <- makeVendorsPerType(year_list[1])
for(year in year_list[-1])
  res = merge(res, makeVendorsPerType(year), by='EquipmentType', all = TRUE)
write.xlsx(res, paste0(output_dir, 'VendorsPerType.xlsx'), row.names=FALSE, showNA=FALSE)

outfile <- paste0(output_dir, 'SpecificNVendors.xlsx')
guarantee.removal(outfile)
for(i in 1:6) {
  res <- makeSpecificNVendors(year_list[1], i)
  for(year in year_list[-1])
    res = merge(res, makeSpecificNVendors(year, i), by='EquipmentType', all = TRUE)
  write.xlsx(res, outfile, sheetName=paste0(i, ' vendor', if(i>1)'s'else''), row.names=FALSE, showNA=FALSE, append=TRUE)
}

message('MOHEV program execution completed.\n')
