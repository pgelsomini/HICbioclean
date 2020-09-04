#To update dependent lookup tables ReferenceSiteLinkage
#and zrxFileStationCodes


#first update the csv file in /data-raw

ReferenceSiteLinkage <- read.csv(data-raw/ReferenceSiteLinkage.csv)

devtools::use_data_raw(ReferenceSiteLinkage, overwrite = T)


#first update the csv file in /data-raw

zrxFileStationCodes <- read.csv(data-raw/zrxFileStationCodes.csv)

devtools::use_data_raw(zrxFileStationCodes, overwrite = T)


