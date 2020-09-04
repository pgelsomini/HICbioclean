# format maintenence files


#' Batch process folder for converting maintenance Excel files to csv files
#'
#' This function was specifically made to convert the excel sensor maintenance files into csv files that can be easily read into R.
#'
#' It batch processes all the files inside a given folder.
#'
#' If you don't provide a full path to the output directory then it will be placed in your working directory.
#'
#' @param input.directory folder directory of Excel files. No back slashes(\), only use forward slashes(/) or double back slashes(\\\\).
#' @param output.directory folder directory where you would like to save the csv files.  No back slashes(\), only use forward slashes(/) or double back slashes(\\\\).
#'
#' @return A folder of csv files with the same name as the input excel files.
#' @export
#'
#' @examples
#' HIC.maint("C:/Rdata/MaintenanceFiles", "MaintenanceFilesCSV")
HIC.maint <- function(input.directory,output.directory){
  require(readxl)

  files <- list.files(input.directory)
  n <- length(files)

  for(i in 1:n){  #load all the files in the InputDirectory. Files 1 to n
    tbl <- read_excel(paste0(input.directory,"/",files[i]),skip = 3) #read the xls file skipping the first 3 lines
    if(!dir.exists(output.directory)) dir.create(output.directory)
    write.csv(tbl,file = paste0(output.directory,"/",files[i],".csv"), row.names = F) #write the csv file
    tbl <-read.csv(paste0(output.directory,"/",files[i],".csv"),header = T,sep = ',',dec = '.')#read the csv file again to turn the tibble into a dataframe in base R
    tbl$DateTimeUNIX <- as.numeric(as.POSIXct(tbl[,1],format="%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-1"))
    write.csv(tbl,file = paste0(output.directory,"/",files[i],".csv"), row.names = F) #write the csv file
  }

}


