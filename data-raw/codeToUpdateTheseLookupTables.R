#To update dependent lookup tables ReferenceSiteLinkage
#and zrxFileStationCodes


#first update the csv file in /data-raw

ReferenceSiteLinkage <- read.csv('data-raw/ReferenceSiteLinkage.csv')

usethis::use_data(ReferenceSiteLinkage, overwrite = T)


#first update the csv file in /data-raw

zrxFileStationCodes <- read.csv('data-raw/zrxFileStationCodes.csv',sep=';',dec='.')

usethis::use_data(zrxFileStationCodes, overwrite = T)


