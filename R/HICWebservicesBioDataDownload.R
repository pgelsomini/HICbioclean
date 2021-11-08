#Function for downloading all biological HIC data from a given year


#' Function for downloading all continuous biological
#' data from the HIC server from a given year
#'
#' Downloads all the HIC data for Chfyla, DO, pH and PPFD for a given year. Additional
#' time series groups can also be added to the download list.
#' The package HICwebservices is required.
#'
#' @param credentials This is the credentials input variable from the HICwebservices package function get_token(). See documentation from HICwebservices package.
#' @param other.group.ids Any other timeseries group IDs that you wish to also download. Take note that the entire group will be downloaded.
#' @param year The year that you wish to download
#' @param output.dir The folder name where you wish all the downloaded files to be saved to. If left as NULL it will by default save to 'DownloadedHICDataYYYYMMDDHHMMSS'
#'
#' @return A folder with all the downloaded time series as csv files
#' and two additional csv files (DownloadSummaryBinary.csv and
#' DownloadSummaryID.csv) that give the summary of which files were downloaded and site coordinates in Belgian Lambert 72.
#' The metadata columns DateTimeUnix, Parameter.Name, Station.Name, Station.Number,
#' Parameter.Unit are added to the downloaded table.
#' @export
HICwebservicesBioDownload <- function(year, credentials = NULL,other.group.ids = NULL, output.dir = NULL) {

  #set year as a string
  year <- as.character(year)

  #Get required package from the HIC
  require(HICwebservices)
  require(reshape2)

  #Get token for downloading data from the HIC if it is not already in the global environment---------------
  tryCatch({
    tokenCheck = check_token()
    if(attr(tokenCheck,'expires')<=Sys.time()) get_token(credentials = NULL)
  },
  error=function(cond){ #if the token has never been downloaded then it will return an error
    get_token(credentials = NULL)
  }
  )

  #download time series----------
  #Chfyla,DO,pH
  ts_id_bio <- 446639
  grouplists <- get_group_list_info(ts_id_bio)

  #custom
  tx_id_custom = other.group.ids
  try(
    for(i in tx_id_custom){
      group_list <- get_group_list_info(i)
      grouplists <- rbind(grouplists, group_list)
    }
  )

  #make a summary table to see what data we have-----
  grouplists1 <- grouplists[,c(4,5,9,10,6,1)]
  grouplistssumaryBinary <- dcast(grouplists1,  station_carteasting+station_cartnorthing+station_name+station_no ~ stationparameter_name, length) # ~ ts_id
  grouplistssumaryTsid <- dcast(grouplists1,  station_carteasting+station_cartnorthing+station_name+station_no ~ stationparameter_name) # ~ ts_id

  #set output directory name----
  if(is.null(output.dir)) {
    output.dir <- paste0('DownloadedHICData', format(Sys.time(), format = '%Y%m%d%H%M%S'))
    message(paste("files saved into", output.dir))
  }
  #make sure output directory exists----
  if(!dir.exists(output.dir)) dir.create(output.dir)

  #export the summary table-------
  write.csv(grouplistssumaryBinary, file = paste0(output.dir,"/DownloadSummaryBinary.csv"), row.names = F)
  write.csv(grouplistssumaryTsid, file = paste0(output.dir,"/DownloadSummaryTsID.csv"), row.names = F)



  #download and export the data to csv------------
  #vector of time series id's

  for(i in 1:nrow(grouplists)){
    ts_id <- grouplists$ts_id[i]
    ts_par <- grouplists$stationparameter_name[i]
    if(ts_par == 'PPFD' | ts_par == 'PPFD1'){ #the PPFD data is too big to take in one go
      ts0 <- get_ts_values(ts_id=ts_id, from = paste0(year,"-01-01 00:00:00"), to = paste0(year,"-03-31 23:59:59"))
      ts1 <- get_ts_values(ts_id=ts_id, from = paste0(year,"-04-01 00:00:00"), to = paste0(year,"-06-30 23:59:59"))
      ts2 <- get_ts_values(ts_id=ts_id, from = paste0(year,"-07-01 00:00:00"), to = paste0(year,"-09-30 23:59:59"))
      ts3 <- get_ts_values(ts_id=ts_id, from = paste0(year,"-10-01 00:00:00"), to = paste0(year,"-12-31 23:59:59"))
      ts <- rbind(ts0,ts1,ts2,ts3)
    }else{
      ts <- get_ts_values(ts_id=ts_id, from = paste0(year,"-01-01 00:00:00"), to = paste0(year,"-12-31 23:59:59"))
    }
    #add metadata as columns
    ts$DateTimeUnix <- as.numeric(ts$Timestamp)
    try({
      ts$Parameter.Name <- NA
      ts$Station.Name <- NA
      ts$Station.Number <- NA
      ts$Parameter.Unit <- NA
    })
    try(ts$Parameter.Name <- grouplists$stationparameter_name[i])
    try(ts$Station.Name <- grouplists$station_name[i])
    try(ts$Station.Number <- grouplists$station_no[i])
    try(ts$Parameter.Unit <- grouplists$ts_unitsymbol[i])

    write.csv(ts, file = paste0(output.dir,'/',grouplists$station_no[i],'_',grouplists$stationparameter_name[i],'_',ts_id,'_',year,'.csv'),row.names = F)
  }
}



#' Function for downloading all continuous abiological
#' data from the HIC server from a given year
#'
#' Downloads all the HIC data for Q(daily average), Turb, conductivity,
#' and temp for a given year. Additional
#' time series groups can also be added to the download list.
#' The package HICwebservices is required.
#'
#' @param credentials This is the credentials input variable from the HICwebservices package function get_token(). See documentation from HICwebservices package.
#' @param other.group.ids Any other timeseries group IDs that you wish to also download. Take note that the entire group will be downloaded.
#' @param year The year that you wish to download
#' @param output.dir The folder name where you wish all the downloaded files to be saved to. If left as NULL it will by default save to 'DownloadedHICDataYYYYMMDDHHMMSS'
#'
#' @return A folder with all the downloaded time series as csv files
#' and two additional csv files (DownloadSummaryBinary.csv and
#' DownloadSummaryID.csv) that give the summary of which files were downloaded.
#' The metadata columns DateTimeUnix, Parameter.Name, Station.Name, Station.Number,
#' Parameter.Unit are added to the downloaded table.
#' @export
HICwebservicesAbioDownload <- function(year, credentials = NULL,other.group.ids = NULL, output.dir = NULL) {

  #set year as a string
  year <- as.character(year)

  #Get required package from the HIC
  require(HICwebservices)
  require(reshape2)

  #Get token for downloading data from the HIC if it is not already in the global environment---------------
  tryCatch({
    tokenCheck = check_token()
    if(attr(tokenCheck,'expires')<=Sys.time()) get_token(credentials = NULL)
  },
  error=function(cond){ #if the token has never been downloaded then it will return an error
    get_token(credentials = NULL)
  }
  )

  #download time series----------

  #Q,Turb,temp,cond
  grouplists <- get_group_list_info(156169)
  ts_id_abio <- c(156202,156200,156173)
  for(i in ts_id_abio){
    group_list <- get_group_list_info(i)
    grouplists <- rbind(grouplists, group_list)
  }
  #custom
  tx_id_custom = other.group.ids
  try(
    for(i in tx_id_custom){
      group_list <- get_group_list_info(i)
      grouplists <- rbind(grouplists, group_list)
    }
  )

  #make a summary table to see what data we have-----
  grouplists1 <- grouplists[,c(4,5,9,10,6,1)]
  grouplistssumaryBinary <- dcast(grouplists1,  station_carteasting+station_cartnorthing+station_name+station_no ~ stationparameter_name, length) # ~ ts_id
  grouplistssumaryTsid <- dcast(grouplists1,  station_carteasting+station_cartnorthing+station_name+station_no ~ stationparameter_name) # ~ ts_id

  #set output directory name----
  if(is.null(output.dir)) {
    output.dir <- paste0('DownloadedHICData', format(Sys.time(), format = '%Y%m%d%H%M%S'))
    message(paste("files saved into", output.dir))
  }
  #make sure output directory exists----
  if(!dir.exists(output.dir)) dir.create(output.dir)

  #export the summary table-------
  write.csv(grouplistssumaryBinary, file = paste0(output.dir,"/DownloadSummaryBinary.csv"), row.names = F)
  write.csv(grouplistssumaryTsid, file = paste0(output.dir,"/DownloadSummaryTsID.csv"), row.names = F)



  #download and export the data to csv------------
  #vector of time series id's




  for(i in 1:nrow(grouplists)){
    ts_id <- grouplists$ts_id[i]
    ts <- get_ts_values(ts_id=ts_id, from = paste0(year,"-01-01 00:00:00"), to = paste0(year,"-12-31 23:59:59"))
    #add metadata as columns
    ts$DateTimeUnix <- as.numeric(ts$Timestamp)
    try({
      ts$Parameter.Name <- NA
      ts$Station.Name <- NA
      ts$Station.Number <- NA
      ts$Parameter.Unit <- NA
    })
    try(ts$Parameter.Name <- grouplists$stationparameter_name[i])
    try(ts$Station.Name <- grouplists$station_name[i])
    try(ts$Station.Number <- grouplists$station_no[i])
    try(ts$Parameter.Unit <- grouplists$ts_unitsymbol[i])

    write.csv(ts, file = paste0(output.dir,'/',grouplists$station_no[i],'_',grouplists$stationparameter_name[i],'_',ts_id,'_',year,'.csv'),row.names = F)
  }
}



