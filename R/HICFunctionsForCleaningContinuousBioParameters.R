#Functions for data cleaning of continuous biological parameters for HIC




#§Function Import and format data from HIC csv files ------------------------------------------------------


#' Batch process: Format data exported from the HIC database
#'
#' Bach process function for importing the continuous water quality data from the HIC Hydrological Information Center(HIC) database.
#' This function takes the csv file that is exported from the HIC data base and converts it into a format that can be used easily in R.
#' The header of the HIC csv file has horizontally oriented metadata. These meta data are taken from the header and put into columns in the dataset.
#' The dates and times are converted into R friendly datetime format and into UNIX numeric format for easier handling in R.
#'
#' Place all HIC csv files into one directory. Specify this InputDirectory in the function in quotes and with forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\).
#' Specify the OutputDirectory where you would like to have the data be exported to  in quotes and with forward-slashes(/) or double-back-slashes(\\\\) no back-slashes(\).
#' If you copy the directory path from windows, it will have back-slashes(\) and these need to be changed to forward-slashes(/) or double-back-slashes(\\\\).
#' If you don't write the full path for the OutputDirectory, then it will create that directory in your working directory.
#'
#' \subsection{Assumed input data file structure}
#' Data table format assumed to be a vertical list of the meta data on top of the horizontally oriented data table. \cr
#' Example of the assumed data table structure of the input data: \cr
#'
#' Station.Number: RTZ25 \cr
#' Parameter.Name: temp \cr
#' Parameter.Unit: C
#' \tabular{rrr}{Date \tab Value \tab State.of.Value \cr
#' 25/01/2018 \tab 5.6 \tab 110 \cr
#' 25/01/2018 \tab 7.8 \tab 110 \cr
#' 25/01/2018 \tab 4.2 \tab 110
#' }
#'
#' \subsection{Possible issues}
#' This code can't deal with extremely inconsistent column names between files. It searches for key words to find the columns in the meta data, but if there are no common words between the different files, then it can't find them.
#' Data is all saved with auto-names Station.ParameterName.Year.SystemTimeInSecondsFileNumber.csv so there is a risk of overwriting older data if you run this batch process in a loop and if multiple files are processed within less than a second of each other with the same
#' station and parameter and happen to be the same file number in their folder. This is unlikely to occur but in theory is possible.
#'
#'
#' @param InputDirectory Path to the folder directory containing all the csv tables that you wish to format placed in quotations. Must have no back slashes (\), they must be all forward slashes (/) or double back slashes (\\\\).
#' @param OutputDirectory Path to the directory where you wish to save the formatted data in quotations. Must have no back slashes (\), they must be all forward slashes (/) or double back slashes (\\\\).
#' @param Data.sep The field separator character for the value data. The columns are separated by this character. The default is tab separated "\t".
#' @param Meta.sep The field separator for the metadata values. The columns are separated by this character. The default is colon separated ":".
#' @param Data.header.line It is assumed that the data header is the first line with the most separations. But if not, then the line number of the data header can be specified.
#' @param Dec The decimal character. By default ".".
#' @param DateFormat Character string giving the date format. See the strptime() help file for additional help.
#' @param TimeZone The time zone is by default UTC+1 "Etc/GMT-1". Use OlsonNames() for a list of all time zone names.
#' @param OneYearDataSet If the dataset is only within one calender year, then you can change this to TRUE and the year will be added to the ID and the file name, but if there are more than one calender years in the dataset then a warning message will appear and the year will not be added to the ID or file name.
#' @param ValueColumnNum The column number of the data values. This column has inconsistent naming and thus must be refered to by column number.
#' @param ParamNameColmn The parameter name column name in the meta data. If you enter in new names, then replace all spaces and special characters with "."
#' @param StationNoColmn The station number column name in the meta data. If you enter in new names, then replace all spaces and special characters with "."
#' @param DateColmn Date column name in quotations.
#' @param TimeColmn Time column name in quotations.
#'
#' @return This function returns each seperate csv file in the input directory as a separate comma separated csv file in the output directory with all the metadata placed into columns to the right of the data, date and time merged into one datetime column ("DateTime"), a numeric datetime column ("DateTimeUnix") in UNIX seconds and all parameter values in the column "Value".
#' @export
#'
#' @examples
#' HIC.Continuous.Data.Import.Format(
#'     InputDirectory = "C:/Rdata/originaldata/HICdata",
#'     OutputDirectory = "FormattedHICdata")
#' #the folder "FormattedHICdata" will be created in your working directory since it is not a full path.
HIC.Continuous.Data.Import.Format = function(InputDirectory,OutputDirectory, Data.sep = "\t", Meta.sep =":", Data.header.line = NULL, Dec = ".",DateFormat="%d/%m/%Y",TimeZone = "Etc/GMT-1",OneYearDataSet = F,ValueColumnNum = 3, ParamNameColmn = "Parameter.Name",StationNoColmn = "Station.Number",DateColmn="Date",TimeColmn="Time"){


  #list all files in the InputDirectory
  files <- list.files(InputDirectory)
  #number of files in the InputDirectory
  n <- length(files)


  for(i in 1:n){  #load all the files in the InputDirectory. Files 1 to n

    message(paste('processing file',i,'of',n))

    tryCatch({   #in case of error, catch error and tell me the file name

      #path for file number i
      HICfile <- paste(InputDirectory,files[i],sep = "/")

      #--find data header--
      #if header line not spesified then find it
      if (is.null(Data.header.line)) {
        #read first 100 lines of the text file
        TextFile <- readLines(HICfile, n=100)
        #Finde the line number with the most tabs. Assuming this is the header for the table.
        a <- which.max(lengths(gregexpr(Data.sep,TextFile)))
        #if header line is spesified then save it as variable a
      }else{a<-Data.header.line}

      #--data--
      #load the Data table from the text file skipping all the lines before the header.
      message('loading data')
      DataTable <- read.table(HICfile,skip = (a-1), na.strings = c("","NA"), fill = T, sep=Data.sep,header = T,dec = Dec)
      names(DataTable)[ValueColumnNum] <- 'Value'

      #--check if there is any data. If not then don't procede--
      if (nrow(DataTable)!=0) {

        #--meta data--
        #load meta data table from text file only loading lines befor the header
        message('loading meta data')
        MetaTable <- read.table(HICfile, nrows = (a-1),na.strings = c("","NA"), fill = T, sep=Meta.sep,header = F,dec = Dec)

        message('formatting data table')
        #fix issues with separating meta data by colon ":"
        #When separating by ":", then the URLs get cut up too and there is a space or a tab infront of the data
        #if meta data separated by ":" then fix these issues
        if (Meta.sep == ":") {
          # remove the tab or space in front of the data values
          MetaTable[,2] <- substring(MetaTable[,2],2)
          #the URL had ":" in them which got cut in two as a result. compine the the second and thrid rows again where the URL's were cut.
          MetaTable[,2] <- ifelse(!is.na(MetaTable[,3]),paste(MetaTable[,2],MetaTable[,3], sep = ":"),MetaTable[,2])
          #delete the third column where the secold half of the URLs used to be
          MetaTable <- MetaTable[,-3]
        }

        #Rotate Metadata Table
        # use t() function to rotate table and then save as a temporary csv file
        write.csv2(t(MetaTable),file = "deleteme66667.csv", row.names = F)
        # reread the teporary csv file skipping the first line which is the old colum titles (V1,V2,V3,...). This automatically formats the new column titles to have proper characters (no spaces or special charicters)
        MetaTable <- read.table("deleteme66667.csv",sep = ";",skip = 1, header = T)
        # delete the temporary CSV file
        unlink("deleteme66667.csv")

        #--add ID fields
        #add ID field "Station.ParameterName.Year" to data table
        if(OneYearDataSet == T) {
          testyears <- as.factor(na.omit(format(DataTable[,grepl(DateColmn,names(DataTable))],"%Y")))
          if(length(levels(na.omit(testyears)))>1){
            warning("There are multiple years of data in this dataset. Year will not be added to the ID.")
            DataTable$ID <- paste(MetaTable[,grepl(StationNoColmn,names(MetaTable))],MetaTable[,grepl(ParamNameColmn,names(MetaTable))], sep = ".")
          }else{
            DataTable$ID <- paste(MetaTable[,grepl(StationNoColmn,names(MetaTable))],MetaTable[,grepl(ParamNameColmn,names(MetaTable))],testyears[1], sep = ".")
          }
        }else{
          DataTable$ID <- paste(MetaTable[,grepl(StationNoColmn,names(MetaTable))],MetaTable[,grepl(ParamNameColmn,names(MetaTable))], sep = ".")
        }

        #--add meta data to data table--
        #merge the two datatables on the ID (there should only be one row in metatable anyways)
        message('adding meta data to data table')
        metanames <- names(MetaTable)
        for(j in 1:length(metanames)){
          DataTable[[metanames[j]]] <- MetaTable[,j]
        }

        #--convert date to R format
        message('converting date to r formate')
        datesdata <- DataTable[[DateColmn]]
        datesdata <- as.character(as.Date(datesdata, format=DateFormat))
        DataTable[[DateColmn]] <- datesdata
        #--convert date and time to R format DateTime time zone UTC+1 and to numeric Unix time
        message('convert to date time format')
        timesdata <- DataTable[[TimeColmn]]
        datetimestext <- paste(datesdata,timesdata)
        DateTimedata <- as.POSIXct(datetimestext,format="%Y-%m-%d %H:%M:%S",tz = TimeZone)
        DataTable$DateTime <- DateTimedata
        message('convert date time to numeric format')
        DateTimedataNum <- as.numeric(DateTimedata)
        DataTable$DateTimeUnix <- DateTimedataNum

        #--save data from data table
        message('exporting data table')
        #create the spesified output directory if it doen't exist anymore
        if(!dir.exists(OutputDirectory)) dir.create(OutputDirectory)
        #save file as Station.ParameterName.Year.SystemTimeInSecondsFileNumber.csv
        write.csv(DataTable, file = paste0(OutputDirectory,"/",DataTable$ID[1],".",format(Sys.time(),format='%Y%m%d%H%M%S'),i,".csv"),row.names = F)

        #if data table was empty, then message that the file has no data
      }else{message(paste(files[i],"no data"))}
    },
    #in case of error give file name and error message
    error=function(cond){
      message(paste("File caused error:",files[i]))
      message("Original error:")
      message(cond)
      return(NULL)
    }
    )#end of try catch
  }#end of for loop

}#end of function




#§ function for autovalidating PPFD data ----------------------

#' Batch process: Despike and autovalidate paired PPFD data and calculate light attenuation coefficient kd
#'
#' This function was designed to auto-validate paired continuous PPFD data where the two sensors are
#' placed at a fixed distance from each other to calculate the light attenuation coefficient kd. It
#' takes a folder containing pairs of csv files of upper and lower PPFD data and calculates the light
#' attenuation coefficient based on those PPFD values and performs an auto-validation process on those data.
#'
#' All the algorithms in this function are the same as in the function dspk.DespikingWorkflow.CSVfileBatchProcess().
#' See the documentation for that function for more details.
#'
#' This function can either batch process paired files in an input directory placed in the argument input.directory or
#' it can process two R objects placed in the arguments DataUpper and Datalower.
#' Specify the input directory in quotes and with forward-slashes(/) or double-back-slashes(\\\\) but no back-slashes(\).
#' If you copy the directory path from
#' windows, it will have back-slashes(\) and these need to be changed to
#' forward-slashes(/) or double-back-slashes(\\\\).
#'
#' Input directory must be a folder containing all the csv files of the PPFD data with the upper
#' and lower sensor data files together with unique station names for each pair of files and
#' consistent separate parameter IDs for the upper and lower sensors also in the file names.
#' For example the files in your directory may be station1.PPFD1.csv, station1.PPFD.csv, station2.PPFD1.csv,
#' station2.PPFD.csv where you have two stations (station1 and station2) and you have an upper sensor ID (PPFD1)
#' and a lower sensor ID (PPFD). You must give the ‘UpperSensorParameterName’ and the ‘LowerSensorParameterName’
#' in order for the function to know which files belong to which sensor. This is not a generic
#' function because it assumes that the components of the csv file names are separated by decimals(.)
#' and that when the files are arranged alphabetically, the upper and lower sensor pairs will be next to each other.
#'
#'    Please see the FunctionLogFile.txt that was generated to see any error messages and details about the selected preferences and calculated preferences.
#'
#' \subsection{Default state of value codes}
#' 255 missing data \cr
#' 110 unchecked data  \cr
#' 80 good data \cr
#' 91 deleted, min max filter \cr
#' 92 deleted, despiked \cr
#' 94 deleted, spike in other sensor \cr
#' 95 not deleted, spike in both sensors \cr
#' 96 deleted, spike in kd \cr
#' 97 deleted, negative kd \cr
#'
#' \subsection{General work flow}
#' Preprocess-formatting. Min max filter on PPFD. Despike PPFD data. All values deleted in one sensor dataset must be deleted in the other as well. Restore original values where spikes are in both sensor datasets. Data gap interpolation PPFD (default max gap to interpolate 1 hour). Calculate light attenuation coefficient kd. Delete PPFD values where kd is negative. Remove kd values where PPFD is below detection limit. Despike kd and delete those spikes from both the PPFD data and the kd data. Data gap interpolation PPFD again. Calculate light attenuation kd again. Delete kd values where PPFD is below detection limit.
#'
#' \subsection{Detailed work flow overview}
#'    All data is saved to a folder ‘autoPPFDdespikeYYYYMMDDHHMMSS’ in your working directory.
#' Formatted data is saved to the subfolder ‘preprocFormat’. The columns ‘dspk.Values’, ’dspk.DateTimeNum’, and ’dspk.StateOfValue’ are created to not overwrite the original data. The original data is saved into the new column 'orig.values' for ease of later reference.
#' Unchecked data is given the state of value code 110 (default) and missing data is coded as 255 (default).
#'
#'    PPFD data is first min/max filtered removing unreasonably high and low values (defaults are min 0 corresponding to no light and max 2000 corresponding to full sun). Deleted values are coded as 91 (default).
#'
#'    PPFD data is then despiked. Checked values coded as 80 (default), deleted as 92 (default) and unchecked values are left with their original state of value code. See documentation for dspk.DespikingWorkflow.CSVfileBatchProcess() for details on the despiking algorithm.
#'
#'    Merge the two datasets joining on time. The upper sensors column have the suffix x and the lower sensors columns have the suffix y.
#'
#'    All values that were deleted in either the upper sensor’s dataset or the lower sensors dataset must be deleted in both datasets and coded as 94 (default). This must be done because the two datasets are compared to each other to calculate light attenuation coefficient kd.
#'
#'    If a spike was detected in both the upper and lower datasets, restore their original values and code as 95 (default). Spikes were often found in both datasets simultaneously which means it wasn’t sensor error. These may have been passing clouds or plumes of suspended matter which is important data for understanding the light climate.
#'
#'    Despiked PPFD data is saved to the subfolder ‘step2Despike’.
#'
#'    PPFD data gaps of maximum one hour (default) are linear interpolated. The stat of value codes are not changed. If the state of value code says it was deleted or was missing but there is a value, then it can be assumed that it was interpolated.
#'
#'    Light attenuation coefficient kd is calculated as kd = 1/Δz*ln(E1/E2) where Δz is the distance between sensors in meters 0.4m (default) and E1 is the upper sensor PPFD and E2 is the lower sensor PPFD. Saved to column ‘kd’. Unite m^-1.
#'
#'    Data saved as csv files to the subdirectory ‘step3Interpol.kd’.
#'
#'    Delete all PPFD data values where kd is negative. State of value code 97 (default). Light cannot be greater lower in the water column.
#'
#'    Copy the column ‘kd’ to the new column 'dspk.kd' and remove all values from 'dspk.kd' outside detection limits 1 PPFD (default) for the upper sensor and 0.25 PPFD (default) for the lower sensor. When the light levels approach zero, it becomes too difficult to accurately measure the difference between the upper and lower sensors.
#'
#'    Despike ‘dspk.kd’ and delete those spikes also from the PPFD columns 'dspk.Values.x' and 'dspk.Values.y'. State of value code 96 (default).
#'
#'    Make a unified state of value for kd. Copy ‘dspk.StateOfValue.x’ into the new column ‘dspk.StateOfValue’ and make all 91 and 92 codes the code 94 (these are the default codes).
#'
#'    Interpolate PPFD data gaps of maximum one hour (default) again.
#'
#'    Calculate kd light attenuation coefficient again in column ‘dspk.kd’.
#'
#'    Delete all ‘dspk.kd’ values where the PPFD values are outside the detection limit: upper sensor PPFD is less than 1 (default) and lower sensor is less than 0.25 (default).
#'
#'    Save the final dataset in subfolder ‘step4Despikekd.FinalData’
#'
#' @param input.directoryCharacter string of the path to the folder containing all the
#' csv files that you wish to batch process. This argument may be omitted if you
#' are entering vectors directly into the ‘Value’ and ‘DateTime’ arguments.
#' @param sep Arguments indicating the formatting of the input csv files.
#'  It is the field separator character. Values are separated by this character.
#' By default it is comma ",".
#' @param dec Arguments indicating the formatting of the input csv files.
#' It the character used for decimal points. By default
#' if is a period ".".
#' @param header Arguments indicating the formatting of the input csv files.
#'  It is a logical value indicating if the first line is the column
#' titles. By default it is TRUE.
#' @param DataUpper A dataframe object for the upper sensor. If you only wish to process one data frame, then it can
#' be entered directly from the R environment with this argument. If you enter in an
#' input.directory then 'DataUpper = ' will be ignored and the files from the input directory
#' will be processed.
#' @param DataLower A dataframe object for the lower sensor. If you only wish to process one data frame, then it can
#' be entered directly from the R environment with this argument. If you enter in an
#' input.directory then 'DataLower = ' will be ignored and the files from the input directory
#' will be processed.
#' @param Value If the data is from a csv file or a dataframe, it is a quoted character string indicating the
#' column name or an integer indicating the column number of the column
#' containing the data values that you wish to despike.  Data may also be entered
#' as a single vector object (unquoted) such as ‘Value = mydata$values’ or ‘Value = values’
#' @param val.NAvalue The value indicating an NA value in your input data. If this value is NA, then this argument can be omitted.
#' @param unchecked.state.of.value.code Number indicating that a given value is unchecked. By default 110.
#' @param add.original.data A logical value indicating if the original input data should
#' be included in the output tables. Note that if you input a csv file, then every
#' column in that file will be kept. TRUE by default.
#' @param DateTime If the data is from a csv file, it is a quoted character string indicating the
#' column name or an integer indicating the column number of the column
#' containing the datetime values of the samples.  Data may also be entered as a
#' single vector object (unquoted) such as ‘DateTime = mydata$time’ or
#'  ‘DateTime = time’
#' @param datetime.format Character string giving the datetime format. See the strptime() help file for additional help.
#' @param datetime.timezone Character string giving the time zone of the datetime. By default “GMT”. Use OlsonNames() for a list of all time zones.
#' @param ConditionalMinMaxColumn The column name in quotes or column number or vector object that contains the factor variable to base your conditional min max filter on
#' @param ConditionalMinMaxValues A vector containing the factor values to base the conditional min max filter on
#' @param ConditionalMin A vector containing the condition minimums that correspond to the respective values in ConditionalMinMaxValues
#' @param ConditionalMax A vector containing the condition maximums that correspond to the respective values in ConditionalMinMaxValues
#' @param Min Number giving the minimum reasonable value. All values below this will be deleted.
#' @param Max Number giving the maximum reasonable value. All values above this will be deleted.
#' @param minmax.state.of.value.code Number indicating that the value has been deleted during the min max filter. By default 91.
#' @param sampling.interval As numeric, the time between samples. If you enter NULL then it will calculate it for you. By default NULL.
#' @param despiked.state.of.value.code Number indicating that a given value was deleted during the despiking. By default 92.
#' @param good.state.of.value.code Number indicating that a given value has been check and deemed not a spike during the despiking. By default 80.
#' @param despike.threshold Number indicating the threshold for defining a spike. By default it is 3, which corresponds to 3 median absolute deviations or 3 standard deviations.
#' @param despike.Method Character string "median" or “mean” indicating the method to use for the despiking. By default “median”.
#' @param precision A number indicating the precision of the input values. Interpolated values will be rounded to this precision. If left as NULL then the numbers will be rounded to the largest decimal length found in the data.
#' @param max.gap As numeric, the time span of the maximum data gap you wish to interpolate.
#' @param DeletedSpikeOtherSensor.state.of.value.code State of value code given to values deleted because they were deleted in the other sensor.
#' @param NotDeletedSpikeBothSensors.state.of.value.code State of value code given to values that were spikes in both sensors and thus not deleted.
#' @param kdDespiked.state.of.value.code State of value code given to values that were deleted because they were spikes in the light attenuation coefficient kd.
#' @param DeletedNegativeKd.state.of.value.code State of value code given to values that were deleted because the light attenuation coefficient kd was negative.
#' @param NA.state.of.value.code State of value code given to missing data.
#' @param kddlupper Minimum PPFD value of the upper sensor for calculating light attenuation coefficient kd.
#' @param kddllower Maximum PPFD value of the upper sensor for calculating light attenuation coefficient kd.
#' @param Dist.Sensors Distance between PPFD sensors for calculating light attenuation coefficient kd.
#' @param UpperSensorParameterName Upper sensor parameter name in the input file name.
#' @param LowerSensorParameterName Lower sensor parameter name in the input file name.
#'
#' @return Each pair of input files gets outputted as one csv file.
#' All data is saved to a folder ‘/autoPPFDdespikeYYYYMMDDHHMMSS’ in your working directory.
#' The final data will be in the subfolder /step4Despikekd.FinalData.
#' Upper sensor data has suffix .x and lower sensor data has suffix .y.
#' The original PPFD data is in columns orig.values.x and orig.values.y
#' The despiked PPFD data is in columns dspk.Values.x and dspk.Values.y.
#' The PPFD state of value codes are in columns dspk.StateOfValue.x and dspk.StateOfValue.y
#' The despiked light attenuation coefficient kd values are in column dspk.kd.
#' The state of value codes for kd are in column dspk.StateOfValue.
#' The UNIX seconds datetime is in column dspk.DateTimeNum.
#' @export
#'
#' @examples
#' #HIC data cleaning and validation protocol
#' #Batch process HIC database PPFD files that were formatted
#' #with the HIC.Continuous.Data.Import.Format() function.
#' HIC.PPFDAutoValidation.CSVfileBatchProcess(
#'     input.directory = "C:/Rdata/PPFDdata",
#'     Value = "Value", val.NAvalue = -777, #all values of -777 will be set to NA
#'     DateTime = "DateTimeUnix",
#'     max.gap = 3600) #3600 seconds or 1 hour maximum gap to interpolate
#'
#' #Process the r object tables PPFDuppersensor and PPFDlowersensor on the column "Value".
#' HIC.PPFDAutoValidation.CSVfileBatchProcess(DataUpper = PPFDuppersensor,
#'     DataLower = PPFDlowersensor, Value = "Value")
#'
HIC.PPFDAutoValidation.CSVfileBatchProcess = function(
  input.directory = NULL,
  sep = ',', dec = '.', header = T,
  DataUpper = NULL, #if no input directory is given then it will take these dataframes
  DataLower = NULL,
  #formating function
  Value, val.NAvalue = NULL, unchecked.state.of.value.code = 110, add.original.data = T,
  DateTime = NULL, datetime.format = NULL, datetime.timezone = "GMT",
  #min max
  ConditionalMinMaxColumn = NULL, ConditionalMinMaxValues = NULL, ConditionalMin = NULL, ConditionalMax = NULL,
  Min = 0, Max = 2000, minmax.state.of.value.code = 91,
  #Despike
  sampling.interval = NULL, despiked.state.of.value.code = 92, good.state.of.value.code = 80, despike.threshold = 3, despike.Method = "median",
  #interpolate
  precision = NULL, max.gap = Inf,
  DeletedSpikeOtherSensor.state.of.value.code = 94,
  NotDeletedSpikeBothSensors.state.of.value.code = 95,
  kdDespiked.state.of.value.code = 96,
  DeletedNegativeKd.state.of.value.code = 97,
  NA.state.of.value.code = 255,
  kddlupper = 1,
  kddllower = 0.25,
  Dist.Sensors = 0.40,
  UpperSensorParameterName = 'PPFD1', #these parameter names are looked for in the file names followed by a point. This makes the function not generic.
  LowerSensorParameterName = 'PPFD'
){

  #function to identify type of input and covert to text conserving quotes and vectors
  #It is importaint that you know exactly what was entered into the function arguments
  itqv <- function(x){
    fff<-function(x){
      if(is.null(x)){return("NULL")}
      else if(is.na(x)){return("NA")}
      else if(is.character(x)){return(paste0("'",x,"'"))}
      else{return(x)}
    }
    if(length(x)>1){return(paste0("c(",paste(lapply(x, fff),collapse = ","),")"))
    }else if(length(x)<1&!is.null(x)){return("''")
    }else{return(fff(x))}
  }

  logdata <- '----Log file for PPFD autovalidation batch process function HIC.PPFDAutoValidation.CSVfileBatchProcess()-----'
  logdata <- rbind(logdata,'-------------------------------------------------------------------------------------------------------------')
  logdata <- rbind(logdata,'Below you will find a quick overview of the function, a list of the arguments provided to the function and a work log')
  logdata <- rbind(logdata,'showing the output directory, original CSV files processed, summaries of each step, files-specific calculated arguments and error reports.')
  logdata <- rbind(logdata,'For support and bug reporting please contact PaliFelice.Gelsomini@uantwerpen.be')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,paste('Date and time of data processing:',format(Sys.time(),format = '%Y.%m.%d %H:%M:%S %Z')))
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'---Overview---')
  logdata <- rbind(logdata,'Despiking workflow: preproces-formatting, min max filter on PPFD, despiking PPFD, all values deleted in one sensor dataset must be deleted in the other aswell, ')
  logdata <- rbind(logdata,'restore original values where spikes are in both sensor datasets, data gap interpolation PPFD, calculate light attenuation kd, delete PPFD values where kd is negative, ')
  logdata <- rbind(logdata,'remove kd values where PPFD is below detection limit, despike kd and delete those spikes from both the PPFD data and the kd data, data gap interpolation PPFD again, ')
  logdata <- rbind(logdata,'calculate light attenuation kd again, delete kd values where PPFD is below detection limit.')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,paste('On CSV files in directory:',input.directory))
  if(is.null(input.directory))logdata <- rbind(logdata,paste('On dataframe from R environment:', ifelse(is.data.frame(DataUpper),deparse(substitute(DataUpper)),itqv(DataUpper)),'and',ifelse(is.data.frame(DataLower),deparse(substitute(DataLower)),itqv(DataLower))))
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'Despiking algorithm: With the default “despike.Method” median and the default “despike.threshold” 3: all data points that are more than 3 median absolute deviations ')
  logdata <- rbind(logdata,'away from the median of the 10 surrounding data points (5 before and 5 after) will be deleted. At least 5 surrounding data points is required for the sample to be evaluated. ')
  logdata <- rbind(logdata,'The algorithm will not look farther than 5 sampling intervals before and after the data point, for handling data gaps. If a “sampling.interval” is not provided then it will ')
  logdata <- rbind(logdata,'be calculated as the mode of the interval between samples. The "dspk.StateOfValue" of the deleted values will be set to “despiked.state.of.value.code” (default 92). The ')
  logdata <- rbind(logdata,'"dspk.StateOfValue" of the values that passed the despike test will be set to “good.state.of.value.code” (default 80). ')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'---Function Arguments:---')
  logdata <- rbind(logdata,paste('input.directory =', itqv(input.directory),', DataUpper =',ifelse(is.data.frame(DataUpper),deparse(substitute(DataUpper)),itqv(DataUpper)),', DataLower =',ifelse(is.data.frame(DataLower),deparse(substitute(DataLower)),itqv(DataLower))))
  logdata <- rbind(logdata,paste('Preprocessing:'))
  logdata <- rbind(logdata,paste('UpperSensorParameterName =',itqv(UpperSensorParameterName), ', LowerSensorParameterName =', itqv(LowerSensorParameterName)))
  logdata <- rbind(logdata,paste('sep =',itqv(sep),', dec =', itqv(dec), ', header =', itqv(header),', add.original.data =', itqv(add.original.data)))
  logdata <- rbind(logdata,paste('Value =',itqv(Value), ', val.NAvalue =',itqv(val.NAvalue)))
  logdata <- rbind(logdata,paste('unchecked.state.of.value.code =', itqv(unchecked.state.of.value.code), ', NA.state.of.value.code =', itqv(NA.state.of.value.code)))
  logdata <- rbind(logdata,paste('DateTime =', itqv(DateTime), ', datetime.format =', itqv(datetime.format), ', datetime.timezone =', itqv(datetime.timezone)))
  logdata <- rbind(logdata,paste('Min max filter:'))
  logdata <- rbind(logdata,paste('ConditionalMinMaxColumn =', itqv(ConditionalMinMaxColumn), ', ConditionalMinMaxValues =', itqv(ConditionalMinMaxValues), ', ConditionalMin =', itqv(ConditionalMin), ', ConditionalMax =', itqv(ConditionalMax)))
  logdata <- rbind(logdata,paste('Min =', itqv(Min), ', Max =', itqv(Max), ', minmax.state.of.value.code =', itqv(minmax.state.of.value.code)))
  logdata <- rbind(logdata,paste('Despiking:'))
  logdata <- rbind(logdata,paste('sampling.interval =', itqv(sampling.interval), ', despiked.state.of.value.code =', itqv(despiked.state.of.value.code), ', good.state.of.value.code =', itqv(good.state.of.value.code), ', despike.threshold =', itqv(despike.threshold), ', despike.Method =', itqv(despike.Method)))
  logdata <- rbind(logdata,paste('DeletedSpikeOtherSensor.state.of.value.code =',itqv(DeletedSpikeOtherSensor.state.of.value.code), ', NotDeletedSpikeBothSensors.state.of.value.code =',itqv(NotDeletedSpikeBothSensors.state.of.value.code)))
  logdata <- rbind(logdata,paste('Step 3 data gap interpolation:'))
  logdata <- rbind(logdata,paste('precision =', itqv(precision), ', max.gap =', itqv(max.gap)))
  logdata <- rbind(logdata,'Light attenuation kd:')
  logdata <- rbind(logdata,paste('kddlupper =',itqv(kddlupper), ', kddllower =',itqv(kddllower), ', Dist.Sensors =',itqv(Dist.Sensors)))
  logdata <- rbind(logdata,paste('kdDespiked.state.of.value.code =',itqv(kdDespiked.state.of.value.code), ', DeletedNegativeKd.state.of.value.code =',itqv(DeletedNegativeKd.state.of.value.code)))
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'---Work log---')



  if(is.null(DateTime)){sampling.interval <- 1} #if no datetime data is provided then one will be made by numbering the records, a sampling interval is then not applicable. It needs to be set to one here because the despike function won't know to remove it since a datetime gets created during the formatting function.

  message('--PPFD data auto validation batch proces--')
  if(is.null(input.directory)){ #if no directory is given
    nfiles = 2                  #then we can only process one file
    files <- c('InputDataUpper','InputDataLower')
    if(is.null(DataUpper)|is.null(DataLower)){stop('if no directory of CSV files is given, then seperate dataframes for both the upper and lower sensors must be provided')}
  }else{
    #list files in directory
    files <- list.files(input.directory)
    #number of files in the directory
    f <- length(files)
  }
  files = list.files(input.directory)
  nfiles = length(files) #number of files in the directory
  if(!(nfiles%%2==0)) stop('Uneven number of files in the input directory. This function processes pairs of upper and lower PPFD sensors. Please make sure that all files are present in the input directory.')
  logdata <- rbind(logdata,paste('Files in input directory:', paste(files,collapse = ',')))
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')


  #create a unique directory with the current time to save data into
  CurrentTime <- format(Sys.time(),"%Y%m%d%H%M%S")
  #create directory if it doen't exist, else add a number to the end till it doen't exist
  dir0 <- paste0("autoPPFDdespike",CurrentTime)
  if(dir.exists(dir0)){
    dup <- 1
    while(dir.exists(paste0(dir0,'_',dup))){dup <- dup+1} #loop till directory is unique
    dir0 <- paste0(dir0,'_',dup)
  }
  dir.create(dir0)
  message(paste("created directory:",dir0))
  logdata <- rbind(logdata,paste('All data saved into output directory:', dir0))
  logdata <- rbind(logdata,'')
  logdata <- rbind(logdata,'')

  dir1 <- paste0(dir0,"/preprocFormat")
  dir.create(dir1)
  dir3 <- paste0(dir0,"/step2Despike")
  dir.create(dir3)
  dir4 <- paste0(dir0,"/step3Interpol.kd")
  dir.create(dir4)
  dir5 <- paste0(dir0,"/step4Despikekd.FinalData")
  dir.create(dir5)

  for (i in 1:(nfiles/2)) { #nfiles/2 because the files are processed in pairs
    tryCatch({   #in case of error, catch error and tell me the file name
      message('-------------------')
      message('-------------------')
      message('-------------------')
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'----------------------------')
      message(paste('processing file set',i,'of',nfiles/2))
      #load data
      if(!is.null(input.directory)){ #if directory is given
        UpperAndLowerSensors = c(UpperSensorParameterName,LowerSensorParameterName)
        if(!(strsplit(files[i*2], ".", fixed = TRUE)[[1]][[1]]==strsplit(files[i*2-1], ".", fixed = TRUE)[[1]][[1]])) stop('Station Names do not match. This function takes all files in a folder and processes them 2 by 2 with the assumption that the upper and lower PPFD sensors will have identical station names, and thus will be placed next to eachother when the csv files are ordered alphabetically. Check that only PPFD data files are in the folder, that each pair of sensors has the same station name and that all pairs of sensors are complete.') #Split the file at the first decimal, this should be the station name. Are the station names the same at the two sites?
        if(sort(UpperAndLowerSensors)[1]==LowerSensorParameterName){ #if lower sensor come first alphabetically
          upperfirst<-F
          if(!grepl(paste0(UpperSensorParameterName,'.'),files[i*2],fixed = T)) stop('Sensor parameter name not found in file name. Check that only PPFD data is in the folder and that the same parameter names are used for the upper and lower sensors at each site.') #check that the second file alphabetically is the upper sensor
          if(!grepl(paste0(LowerSensorParameterName,'.'),files[i*2-1],fixed = T)) stop('Sensor parameter name not found in file name. Check that only PPFD data is in the folder and that the same parameter names are used for the upper and lower sensors at each site.') #check that the first file alphabetically is the lower sensor
          T2 = read.csv(paste0(input.directory, '/', files[i*2-1]), sep = sep, dec = dec, header = header) # should be lower sensor
          T1 = read.csv(paste0(input.directory, '/', files[i*2]), sep = sep, dec = dec, header = header)  # should be upper sensor
          logdata <- rbind(logdata,paste('Processing upper sensor file:',files[i*2],'and lower sensor file:',files[i*2-1]))
        }else{
          upperfirst<-T
          if(!grepl(paste0(UpperSensorParameterName,'.'),files[i*2-1],fixed = T)) stop('Sensor parameter name not found in file name. Check that only PPFD data is in the folder and that the same parameter names are used for the upper and lower sensors at each site.') #check that the second file alphabetically is the upper sensor
          if(!grepl(paste0(LowerSensorParameterName,'.'),files[i*2],fixed = T)) stop('Sensor parameter name not found in file name. Check that only PPFD data is in the folder and that the same parameter names are used for the upper and lower sensors at each site.') #check that the first file alphabetically is the lower sensor
          T1 = read.csv(paste0(input.directory, '/', files[i*2-1]), sep = sep, dec = dec, header = header) # should be upper sensor
          T2 = read.csv(paste0(input.directory, '/', files[i*2]), sep = sep, dec = dec, header = header)  # should be lower sensor
          logdata <- rbind(logdata,paste('Processing upper sensor file:',files[i*2-1],'and lower sensor file:',files[i*2]))
        }
      }else{ #if no directory is given
        upperfirst<-T
        T1 = DataUpper
        T2 = DataLower
        if(!is.data.frame(T1)|!is.data.frame(T2)){stop('Entered DataUpper and DataLower must be dataframes')}
        if(is.null(input.directory))logdata <- rbind(logdata,paste('Loaded dataframe from R environment:', ifelse(is.data.frame(DataUpper),deparse(substitute(DataUpper)),itqv(DataUpper)),'and',ifelse(is.data.frame(DataLower),deparse(substitute(DataLower)),itqv(DataLower))))
      }

      #format data
      message('formatting data')
      FT1 = dspk.TableFormatting(Data = T1, Value = Value, DateTime = DateTime, datetime.format = datetime.format, datetime.timezone = datetime.timezone, NAvalue = val.NAvalue, state.of.value.code.na = NA.state.of.value.code, state.of.value.code = unchecked.state.of.value.code, add.original.data = add.original.data)
      FT2 = dspk.TableFormatting(Data = T2, Value = Value, DateTime = DateTime, datetime.format = datetime.format, datetime.timezone = datetime.timezone, NAvalue = val.NAvalue, state.of.value.code.na = NA.state.of.value.code, state.of.value.code = unchecked.state.of.value.code, add.original.data = add.original.data)

      #save formatted data
      message('saving formatted data')
      if(upperfirst){
        write.csv(FT1,paste0(dir1,'/',files[i*2-1],'.formatted.csv'),row.names = F)
        write.csv(FT2,paste0(dir1,'/',files[i*2],'.formatted.csv'),row.names = F)
      }else{
        write.csv(FT1,paste0(dir1,'/',files[i*2],'.formatted.csv'),row.names = F)
        write.csv(FT2,paste0(dir1,'/',files[i*2-1],'.formatted.csv'),row.names = F)
      }

      #save original data values into new columns for later graphical checks and restoring double spikes
      FT1$orig.values = FT1$dspk.Values
      FT2$orig.values = FT2$dspk.Values

      #min max filter
      message('min max filter')
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'Min max filter PPFD')
      if(is.null(Min)){Min = -Inf}
      if(is.null(Max)){Max = Inf}
      #--conditional min max--
      #if all the conditional variables are filled in and the given vectors are all the same length
      if(!is.null(ConditionalMinMaxColumn)&!is.null(ConditionalMinMaxValues)&!is.null(ConditionalMin)&!is.null(ConditionalMax)&
         length(ConditionalMinMaxValues)==length(ConditionalMin)&length(ConditionalMax)==length(ConditionalMin)){
        conditionalvalue <- as.character(dspk.vectorize(ConditionalMinMaxColumn, Data = FT1)[1])#get the conditional value out of the datatable
        minmaxindex <- which(ConditionalMinMaxValues==conditionalvalue)
        if(length(minmaxindex)==1){ #check that there is only and atleast one mach
          Min1<-ConditionalMin[minmaxindex]
          Max1<-ConditionalMax[minmaxindex]
        }else{
          Min1 <- Min
          Max1 <- Max
        }
        if(is.na(Min1)){Min1 = Min}
        if(is.na(Max1)){Max1 = Max}
        message(paste('conditional min max value:',conditionalvalue))
        logdata <- rbind(logdata,paste('conditional min max value:',conditionalvalue))
        message(paste('conditional min:',Min1))
        logdata <- rbind(logdata,paste('conditional min:',Min1))
        message(paste('conditional min:',Max1))
        logdata <- rbind(logdata,paste('conditional min:',Max1))
      }else{
        Min1 = Min
        Max1 = Max
      }
      #if the conditional vector returns an NA because for example the current parameter was not given in the given vectors, then take the non conditional min and max
      if(is.na(Min1)){Min1 = Min}
      if(is.na(Max1)){Max1 = Max}
      Min1 = (as.numeric(Min1))
      Max1 = (as.numeric(Max1))
      MMT1 = dspk.MinMaxfilter(Data = FT1, Value = "dspk.Values", Min = Min1, Max =Max1, State.of.value.data = "dspk.StateOfValue", state.of.value.code = minmax.state.of.value.code, NAvalue = NULL,logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(MMT1$logdata))))
      MMT1 <- as.data.frame(MMT1$data)
      MMT2 = dspk.MinMaxfilter(Data = FT2, Value = "dspk.Values", Min = Min1, Max =Max1, State.of.value.data = "dspk.StateOfValue", state.of.value.code = minmax.state.of.value.code, NAvalue = NULL,logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(MMT2$logdata))))
      MMT2 <- as.data.frame(MMT2$data)

      #spike filter
      message('despiking')
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'Despike PPFD')
      SFT1 = dspk.Spikefilter(Value =  MMT1$dspk.Values, NumDateTime = FT1$dspk.DateTimeNum, sampling.interval = sampling.interval, State.of.value.data = MMT1$dspk.StateOfValue, state.of.value.code = despiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method,logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(SFT1$logdata))))
      SFT1 <- as.data.frame(SFT1$data)
      SFT2 = dspk.Spikefilter(Value =  MMT2$dspk.Values, NumDateTime = FT2$dspk.DateTimeNum, sampling.interval = sampling.interval, State.of.value.data = MMT2$dspk.StateOfValue, state.of.value.code = despiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method,logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(SFT2$logdata))))
      logdata <- rbind(logdata,'')
      SFT2 <- as.data.frame(SFT2$data)

      #save despiked values to formatted dataset
      FT1$dspk.Values = SFT1$dspk.Values
      FT2$dspk.Values = SFT2$dspk.Values
      FT1$dspk.StateOfValue = SFT1$dspk.StateOfValue
      FT2$dspk.StateOfValue = SFT2$dspk.StateOfValue

      #merge data sets by time to garentee matchup of data
      message('merging upper and lower sensor datasets')
      MergeT = merge(x = FT1, y= FT2, by = 'dspk.DateTimeNum')

      #if a variable was removed in one table then delete in both
      message('Deleting value in both upper and lower sensor datasets where spike was found in only one sensor')
      con_tab1_minmaxdspk = (MergeT$dspk.StateOfValue.x == minmax.state.of.value.code|MergeT$dspk.StateOfValue.x == despiked.state.of.value.code) # variables deleted in table 1
      con_tab2_minmaxdspk = (MergeT$dspk.StateOfValue.y == minmax.state.of.value.code|MergeT$dspk.StateOfValue.y == despiked.state.of.value.code) # variables deleted in table 2
      con_tab1 = con_tab2_minmaxdspk & !con_tab1_minmaxdspk #all values that have been deleted from table 2 but not from table 1
      con_tab2 = con_tab1_minmaxdspk & !con_tab2_minmaxdspk#all values that have been deleted from table 1 but not from table 2
      MergeT$dspk.StateOfValue.x[con_tab1] = DeletedSpikeOtherSensor.state.of.value.code #change state of value to DeletedSpikeOtherSensor.state.of.value.code for samples that were deleted in the other table
      MergeT$dspk.StateOfValue.y[con_tab2] = DeletedSpikeOtherSensor.state.of.value.code
      MergeT$dspk.Values.x[con_tab1] = NA #delete those values that were deleted in the other table
      MergeT$dspk.Values.y[con_tab2] = NA
      logdata <- rbind(logdata,paste(sum(con_tab1),'values were deleted (min/max and spikes) from the lower sensor but not the upper sensor. These values are now also deleted from the upper sensor.'))
      logdata <- rbind(logdata,paste(sum(con_tab2),'values were deleted (min/max and spikes) from the upper sensor but not the lower sensor. These values are now also deleted from the lower sensor.'))

      #if a record was despiked in both sensors then don't delete them
      message('Restoring original values to spikes found in both upper and lower sensors')
      con_tab1_dspk = (MergeT$dspk.StateOfValue.x == despiked.state.of.value.code) # records despiked in table 1
      con_tab2_dspk = (MergeT$dspk.StateOfValue.y == despiked.state.of.value.code) # records despiked in table 2
      con = con_tab1_dspk & con_tab2_dspk
      MergeT$dspk.StateOfValue.x[con] = NotDeletedSpikeBothSensors.state.of.value.code #change state of value to NotDeletedSpikeBothSensors.state.of.value.code for samples that were spikes in both tables
      MergeT$dspk.StateOfValue.y[con] = NotDeletedSpikeBothSensors.state.of.value.code
      MergeT$dspk.Values.x[con] = MergeT$orig.values.x[con] #reinstate the original values
      MergeT$dspk.Values.y[con] = MergeT$orig.values.y[con]
      logdata <- rbind(logdata,paste(sum(con),'values were spikes in both the upper and lower sensor datasets. Their original values have been restored.'))

      #save despiked data
      message('saving initial PPFD despiked data')
      write.csv(MergeT,paste0(dir3,'/merged.',files[i*2-1],'.minmax.despiked.csv'),row.names = F)

      #gap interpolation of gaps
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'interpolating gaps in PPFD data')
      message('interpolating gaps in PPFD data')
      GIT1 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.x', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.x', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(GIT1$logdata))))
      GIT1 <- as.data.frame(GIT1$data)
      GIT2 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.y', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.y', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(GIT2$logdata))))
      GIT2 <- as.data.frame(GIT2$data)

      #save interpolated data to the merged table
      MergeT$dspk.Values.x = GIT1$dspk.Values
      MergeT$dspk.Values.y = GIT2$dspk.Values
      #I will not add the state of value codes from the interpolation to the new data so that i can see where the interpolated data came from

      #calculating kd light attenuation coeficient
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'Calculate kd')
      message('calculating kd between upper and lower sensors')
      # kd = 1/Δz*ln(E1/E2) where Δz is the distance between sensors 40cm and E1 is the upper sensor and E2 is the lower sensor
      #MergeT$PPFDdiff = MergeT$dspk.Values.x - MergeT$dspk.Values.y
      MergeT$kd = 1/Dist.Sensors*log(MergeT$dspk.Values.x / MergeT$dspk.Values.y) #calculating light attenuation coeficient

      #save initial kd data
      message('saving initial interpolated and kd data')
      write.csv(MergeT,paste0(dir4,'/merged.',files[i*2-1],'.minmax.despiked.interpol.diff.kd.csv'),row.names = F)

      #Delete all PPFD data values where kd is negative
      #light cannot be greater lower in the water column
      message('deleting all PPFD values where kd is negative')
      con <- MergeT$kd < 0 & !is.na(MergeT$kd)
      MergeT$dspk.kd <- MergeT$kd #save original kd into a new cloumn
      MergeT$dspk.kd[con] <- NA
      MergeT$dspk.Values.x[con] <- NA
      MergeT$dspk.Values.y[con] <- NA
      MergeT$dspk.StateOfValue.x[con] <- DeletedNegativeKd.state.of.value.code
      MergeT$dspk.StateOfValue.y[con] <- DeletedNegativeKd.state.of.value.code
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,paste(sum(con),'kd values were negative. These data points were deleted from the PPFD data.'))

      ##do not report all kd values where upper sensor is less than 1 and lower sensor is less than 0.25
      message('removing all kd values where PPFD is outside of detection limits for kd')
      con <- (MergeT$dspk.Values.x < kddlupper | MergeT$dspk.Values.y < kddllower ) & !is.na(MergeT$dspk.Values.x) & !is.na(MergeT$dspk.Values.y)
      MergeT$dspk.kd[con] <- NA
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,paste(sum(con),'values had PPFD values below the detection limits ( kddllower =',kddllower,', kddlupper =',kddlupper,') for calculating reliable kd values. These kd values were deleted.'))

      #despike kd and delete those spikes also from PPFD
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'despike kd and remove PPFD values where kd spikes')
      message('despiking kd and remove PPFD values where kd spikes')
      kddspk <- dspk.Spikefilter(Value =  MergeT$dspk.kd, NumDateTime = MergeT$dspk.DateTimeNum, sampling.interval = sampling.interval, State.of.value.data = MergeT$dspk.StateOfValue.x, state.of.value.code = kdDespiked.state.of.value.code, good.state.of.value.code = good.state.of.value.code, NAvalue = NULL, threshold = despike.threshold, Method = despike.Method, logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(kddspk$logdata))))
      kddspk <- as.data.frame(kddspk$data)
      con <- kddspk$dspk.StateOfValue == kdDespiked.state.of.value.code #spike in kd

      MergeT$dspk.kd[con] <- NA
      MergeT$dspk.Values.x[con] <- NA
      MergeT$dspk.Values.y[con] <- NA
      MergeT$dspk.StateOfValue.x[con] <- kdDespiked.state.of.value.code
      MergeT$dspk.StateOfValue.y[con] <- kdDespiked.state.of.value.code

      #make a unified state of value for kd
      #255 missing
      #110 not evaluated
      #80 good
      #94 deleted (either min/max or spike in one sensor)
      #95 spike in both sensors not deleted
      #96 deleted spike in kd
      #97 negative kd value
      MergeT$dspk.StateOfValue <- MergeT$dspk.StateOfValue.x
      con <- MergeT$dspk.StateOfValue.x == minmax.state.of.value.code |MergeT$dspk.StateOfValue.x == despiked.state.of.value.code
      MergeT$dspk.StateOfValue[con]<- DeletedSpikeOtherSensor.state.of.value.code #rename all the deleted records during the PPFD despikign DeletedSpikeOtherSensor.state.of.value.code

      #interpolate PPFD again
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'interpolate PPFD data gaps again')
      message('interpolating PPFD data gaps again')
      GIT1 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.x', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.x', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(GIT1$logdata))))
      GIT1 <- as.data.frame(GIT1$data)
      GIT2 = dspk.DataGapInterpolation (Data = MergeT, Value = 'dspk.Values.y', precision = precision, NumDateTime = 'dspk.DateTimeNum', max.gap = max.gap, State.of.value.data = 'dspk.StateOfValue.y', state.of.value.code = 93, NAvalue = NULL, logoutput = T)
      logdata <- rbind(logdata,t(t(unlist(GIT2$logdata))))
      GIT2 <- as.data.frame(GIT2$data)
      #save interpolated data to the merged table
      MergeT$dspk.Values.x = GIT1$dspk.Values
      MergeT$dspk.Values.y = GIT2$dspk.Values
      #I will not add the state of value codes from the interpolation to the new data so that i can see where the interpolated data came from

      #calculating kd light attenuation coeficient again
      # kd = 1/Δz*ln(E1/E2) where Δz is the distance between sensors 40cm and E1 is the upper sensor and E2 is the lower sensor
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'Recalculate kd')
      message('recalculating kd between upper and lower sensors')
      #MergeT$dspk.PPFDdiff = MergeT$dspk.Values.x - MergeT$dspk.Values.y
      MergeT$dspk.kd <- 1/Dist.Sensors*log(MergeT$dspk.Values.x / MergeT$dspk.Values.y) #calculating light attenuation  coefficient
      MergeT$dspk.kd <- round(MergeT$dspk.kd,digits = 4)

      ##do not report all kd values where upper sensor is less than 1 and lower sensor is less than 0.25
      message('removing all kd values where PPFD is outside of detection limits for kd')
      con <- (MergeT$dspk.Values.x < kddlupper | MergeT$dspk.Values.y < kddllower) & !is.na(MergeT$dspk.Values.x) & !is.na(MergeT$dspk.Values.y)& !is.na(MergeT$dspk.kd)
      MergeT$dspk.kd[con] <- NA
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,paste(sum(con),'values had PPFD values below the detection limits (kddllower =',kddllower,',kddlupper =',kddlupper,') for calculating reliable kd values. These kd values were deleted.'))
      logdata <- rbind(logdata,'')
      logdata <- rbind(logdata,'')

      #save final data
      message('saving final dataset')
      write.csv(MergeT,paste0(dir5,'/merged.',files[i*2-1],'.minmax.despiked.interpol.diff.kd.kddespiked.csv'),row.names = F)


    },
    #in case of error give file name and error message
    error=function(cond){
      message(paste("Files caused error:",files[i*2-1],'and',files[i*2]))
      message(paste("Original error:",cond))
      logdata <<- rbind(logdata,paste("Files caused error:",files[i*2-1],'and',files[i*2]))
      logdata <<- rbind(logdata,paste("Original error:",cond))
      #return(NULL)
    }
    )#end of try catch

  }

  fileConn<-file(paste0(dir0,"/FunctionLogFile.txt"))
  writeLines(logdata, fileConn)
  close(fileConn)

}

