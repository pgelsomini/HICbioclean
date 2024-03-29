% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpikeRemovalFunctions.R
\name{dspk.DespikingWorkflow.CSVfileBatchProcess}
\alias{dspk.DespikingWorkflow.CSVfileBatchProcess}
\title{Batch process: Despike and autovalidate continuous data}
\usage{
dspk.DespikingWorkflow.CSVfileBatchProcess(
  steps = c(1, 2, 3),
  input.directory = NULL,
  sep = ",",
  dec = ".",
  header = T,
  Data = NULL,
  Value,
  val.NAvalue = NULL,
  unchecked.state.of.value.code = 110,
  NA.state.of.value.code = 255,
  add.original.data = T,
  DateTime = NULL,
  datetime.format = NULL,
  datetime.timezone = "GMT",
  ConditionalMinMaxColumn = NULL,
  ConditionalMinMaxValues = NULL,
  ConditionalMin = NULL,
  ConditionalMax = NULL,
  Min = (-Inf),
  Max = Inf,
  minmax.state.of.value.code = 91,
  sampling.interval = NULL,
  despiked.state.of.value.code = 92,
  good.state.of.value.code = 80,
  despike.threshold = 3,
  despike.Method = "median",
  precision = NULL,
  max.gap = Inf
)
}
\arguments{
\item{steps}{Numeric vector containing the values 1, 2 and/or 3 corresponding to
step 1 min max filter, step 2 despiking and step 3 gap interpolation. For
example ‘steps=c(1,3)’ will run a min max filter and  then interpolate the gaps.
Will run all steps by default.}

\item{input.directory}{Character string of the path to the folder containing all the
csv files that you wish to batch process. This argument may be omitted if you
are entering vectors directly into the ‘Value’ and ‘DateTime’ arguments.}

\item{sep}{Arguments indicating the formatting of the input csv files.
 It is the field separator character. Values are separated by this character.
By default it is comma ",".}

\item{dec}{Arguments indicating the formatting of the input csv files.
It the character used for decimal points. By default
if is a period ".".}

\item{header}{Arguments indicating the formatting of the input csv files.
It is a logical value indicating if the first line is the column
titles. By default it is TRUE.}

\item{Data}{A dataframe object. If you only wish to process one data frame, then it can
be entered directly from the R environment with this argument. If you enter in an
input.directory then 'Data = ' will be ignored and the files from the input directory
will be processed.}

\item{Value}{If the data is from a csv file or a dataframe, it is a quoted character string indicating the
column name or an integer indicating the column number of the column
containing the data values that you wish to despike.  Data may also be entered
as a single vector object (unquoted) such as ‘Value = mydata$values’ or ‘Value = values’}

\item{val.NAvalue}{The value indicating an NA value in your input data. If this value is NA, then this argument can be omitted.}

\item{unchecked.state.of.value.code}{Number indicating that a given value is unchecked. By default 110.}

\item{NA.state.of.value.code}{State of value code given to missing data.}

\item{add.original.data}{A logical value indicating if the original input data should
be included in the output tables. Note that if you input a csv file, then every
column in that file will be kept. TRUE by default.}

\item{DateTime}{If the data is from a csv file, it is a quoted character string indicating the
column name or an integer indicating the column number of the column
containing the datetime values of the samples.  Data may also be entered as a
single vector object (unquoted) such as ‘DateTime = mydata$time’ or
 ‘DateTime = time’}

\item{datetime.format}{Character string giving the datetime format. See the strptime() help file for additional help.}

\item{datetime.timezone}{Character string giving the time zone of the datetime. By default “GMT”.  Use OlsonNames() for a list of all time zones.}

\item{ConditionalMinMaxColumn}{The column name in quotes or column number or vector object that contains the factor variable to base your conditional min max filter on}

\item{ConditionalMinMaxValues}{A vector containing the factor values to base the conditional min max filter on}

\item{ConditionalMin}{A vector containing the condition minimums that correspond to the respective values in ConditionalMinMaxValues}

\item{ConditionalMax}{A vector containing the condition maximums that correspond to the respective values in ConditionalMinMaxValues}

\item{Min}{Number giving the minimum reasonable value. All values below this will be deleted.}

\item{Max}{Number giving the maximum reasonable value. All values above this will be deleted.}

\item{minmax.state.of.value.code}{Number indicating that the value has been deleted during the min max filter. By default 91.}

\item{sampling.interval}{As numeric, the time between samples. If you enter NULL then it will calculate it for you. By default NULL.}

\item{despiked.state.of.value.code}{Number indicating that a given value was deleted during the despiking. By default 92.}

\item{good.state.of.value.code}{Number indicating that a given value has been check and deemed not a spike during the despiking. By default 80.}

\item{despike.threshold}{Number indicating the threshold for defining a spike. By default it is 3, which corresponds to 3 median absolute deviations or 3 standard deviations.}

\item{despike.Method}{Character string "median" or “mean” indicating the method to use for the despiking. By default “median”.}

\item{precision}{A number indicating the precision of the input values. Interpolated values will be rounded to this precision. If left as NULL then the numbers will be rounded to the largest decimal length found in the data.}

\item{max.gap}{As numeric, the time span of the maximum data gap you wish to interpolate}
}
\value{
Your final data can be found in “autodespikeYYYYMMDDHHMMSS/step3Interpol.FinalData” within your
working directory as comma separated csv files. The cleaned values will be in column "dspk.Values", the state of values in column "dspk.StateOfValue", and the numeric datetime will be in column "dspk.DateTimeNum".
}
\description{
A full work flow for auto validation of continuous data. It may be run as a batch
process or on individual R objects. It performs min max filtering, despiking and
linear gap interpolation. You may run all these steps or a select few.
Files are all outputted as csv files to your working directory at each step.
}
\details{
Each csv file will be process separately and saved into new csv files at each
step in the despiking process (pre-process: formatting, step 1: min/max filter,
step 2: despiking, step 3: gap interpolation). The final data will be found
in the folder “autodespikeYYYYMMDDHHMMSS” within the subfolder
“step3Interpol.FinalData”. Use the function
getwd() to find your working directory. State of value codes are added to the
data to keep track of how each value was handled during the auto-validation
process (110 unchecked, 255 missing, 80 auto good value, 91 deleted during min/max filter,
92 deleted during despiking). The original data will still be
in the newly generated csv files, with the processed data saved in new
columns.

Please see the FunctionLogFile.txt that was generated to see any error messages and details about the selected preferences and calculated preferences.

\subsection{Quick Start}
Place all your data you wish to auto-despike into one folder as csv files. The
data values must be numeric. Date and time should be both in the same
column with no time zone corrections (e.g. 13:20 +2   The +2 is a time zone
correction). Datetime may also be numeric. If there are no interruptions in the
sampling causing data gaps, then a datetime is not needed.

\subsection{The default state of values codes}
110 Unchecked \cr
255 Missing value \cr
80 Auto good \cr
91 Auto delete min max filter \cr
92 Auto delete Spike \cr

\subsection{Algorithm overview and workflow}
Pre-processing: Formatting and compatibility check: If an
‘input.directory’ containing multiple csv files was provided, then each file will
be processed and saved separately. The ‘Value’ data is checked that it is
numeric and are then saved into a new column "dspk.Values" to not overwrite
old data. NA codes ‘val.NAvalue’ will be replaced with the value NA. The
 ‘DateTime’ data will be checked if it is numeric and saved into a new column
"dspk.DateTimeNum" to not overwrite old data. If it is a character string, then
it will be converted to numeric using the provided ‘datetime.format’ and
 ‘datetime.timezone’. If no datetime is provided then the samples will be
numbered consecutively and saved as the datetime. A new column
"dspk.StateOfValue" will be generated with all values equal to the
 ‘unchecked.state.of.value.code’ (default 110). NA values will be given the
 'NA.state.of.value.code' (default 255). If the entered csv data table
already has a "dspk.StateOfValue" column, then the original state of values
from that column will be used. If ‘add.original.data’ is equal to TRUE (this is the
default) then the original data will be included in the formatted data table. The
formatted data table will be saved to a new csv file in the directory
 “autodespikeYYYYMMDDHHMMSS/preprocFormat” within your working
directory.

Step 1: Min/Max filter: Each csv file generated from the previous step will be
processed and saved separately. All data points that are above the entered
 ‘Max’ or below entered ‘Min’ will be deleted. The "dspk.StateOfValue" of the
deleted values will be set to ‘minmax.state.of.value.code’ (default 91). The
data will be saved in a new csv file in the directory
“autodespikeYYYYMMDDHHMMSS/step1MinMax” within your working
directory.

Step 2: Despiking: Each csv file generated from the previous step will be
processed and saved separately. With the default “despike.Method” median
and the default “despike.threshold” 3: all data points that are more than 3
median absolute deviations away from the median of the 10 surrounding data
points (5 before and 5 after) will be deleted. At least 5 surrounding data points
is required for the sample to be evaluated. The algorithm will not look farther
than 5 sampling intervals before and after the data point, for handling data gaps.
If a “sampling.interval” is not provided then it will be calculated as the mode of
the interval between samples. The "dspk.StateOfValue" of the
deleted values will be set to “despiked.state.of.value.code” (default 92). The
"dspk.StateOfValue" of the values that passed the despike test will be set to
“good.state.of.value.code” (default 80). The data will be saved in a new csv file
in the directory  “autodespikeYYYYMMDDHHMMSS/step2Despike” within your
working directory.'

Step 3: Data gap interpolation: Each csv file generated from the previous step
will be processed and saved separately. All data gaps will be linear
interpolated unless a ‘max.gap’ length for interpolation is given. If a ‘precision’
is given, then the interpolated values will be rounded to that precision. The
state of values will not be changed to know what the original state of the value was.
If the value has a state of value deleted or missing but there is a value then it can
be assumed that it was interpolated. The data will be saved in a new
csv file in the directory
 “autodespikeYYYYMMDDHHMMSS/step3Interpol.FinalData” within your
working directory.

\subsection{Details}
Make sure that when you enter the path name for the directory it has
forward-slashes(/) or double-back-slashes(\\\\) and not back-slashes(\). If you copy the directory path from
windows, it will have back-slashes(\) and these need to be changed to
forward-slashes(/) or double-back-slashes(\\\\).

Interpolated the values will be rounded to the same decimal places
as the original data. If you wish you may enter a custom precision.
Examples: 0.566 would be ‘precision = 0.001’ 1200 would be ‘precision = 100’
Measurements with steps of 5 would be ‘precision = 5’ Measurements to the
nearest half unit would be ‘precision = 0.5’

You are not required to supply ‘DateTime’ for this function. This is only
necessary for handling data gaps while despiking and interpolating the data. If
you omit this argument, then it will generate a datetime column which
contains the samples numbered consecutively so you can still indicate with
 ‘max.gap’ the maximum data gap you wish to interpolate by filling in the
number of missing samples into ‘max.gap’. The ‘DateTime’ data can be
numeric values or as a datetime character strings (e.g. “2018-04-23 15:32:18”).
If the datetime data are character strings, then a ‘datetime.format’ must be
provided (e.g. datetime.format = "%Y-%m-%d %H:%M:%S"). See the strptime()
function documentation for help on syntax. If you enter a date time as a
character string, then it will be converted into UNIX seconds. The default time
zone is GMT but it can be changed to GMT+1 with 'datetime.timezone =
"Etc/GMT-1"' Use the OlsonNames() function for a list of all time zones. If you enter
a datetime, then the date and time must be in the same cell, as in they cannot
be in separate columns. Often a time conversion to GMT is supplied with the
time (e.g. 16:32 +2). This function uses the as.POSIXct function and cannot
handle these conversions (like the "+2" in the example). They need to be
removed and dealt with prior to analysis.

When the data is being formatted the columns "dspk.Values",
"dspk.DateTimeNum" and "dspk.StateOfValue" will be generated. If these
columns already exist in the data table, then they will be overwritten. This may
or may not be desired. If the column "dspk.StateOfValue" is in the original
data, then that data will be used for the 'state of values', otherwise the
'unchecked.state.of.value.code' (default 110) will be used for all data signifying 'unchecked' status.

The spike removal algorithm is by default (despike.Method = "median") all
sample points that are more than the threshold 3 median absolute deviations
 (the scale factor 1.4826 is used assuming normal distribution) from the
median of the 10 surrounding data points (5 before and 5 after) are
automatically deleted. The threshold of 3 can be changed with
"despike.threshold =" You can set despike.Method = "mean" to use standard
deviations and mean for the algorithm instead of the default median absolute
deviations and median. However median is a much more robust statistic for
handling outliers.

If you have data gaps and you have the sampling times and you don”t want the
despiking algorithm to look past the data gaps, then make sure to to supply
“DateTime”. The time interval between samples "sampling.interval = " will be
calculated as the mode of the difference between consecutive samples. If there
are irregularities in the sampling interval that will prevent this calculation
then you can enter in the sampling interval in "sampling.interval = " in the
numeric-datetime unit. If you entered in a character datatime column then it is
POSIX time converted to numeric so the unit is in UNIX seconds.

The default is that it will do linear interpolation of all data gaps. You can
restrict the size of the data gap with "max.gap.interpolate". This needs to be in
the unit of your numeric datetime. If you entered in a datetime column
containing character strings then it is POSIX time converted to numeric so the
unit is in UNIX seconds. If you did not entered a time column, then the unit is in
samples, for example "max.gap.interpolate = 5" will only interpolate gaps of
up to 5 samples long.
}
\examples{
#HIC data cleaning and validation protocol
#Batch process HIC database files that were formatted
#with the HIC.Continuous.Data.Import.Format() function.
#With default despiking algorithm (threshold of 3 MAD from the median).
dspk.DespikingWorkflow.CSVfileBatchProcess(
   input.directory = 'data/HIC.data', #Load csv files from the folder 'HIC.data'
   sep = ',', dec = '.', header = T,  #The csv files are separated by commas with
   #point decimals and the first line is the column names.
   Value = 3, val.NAvalue = -777, #The values are found in the third column.
   #-777 stands for no-value
   DateTime = "DateTimeUnix",
   #The numeric datetime column name.
   #Because it is numeric no datetime formatting info is needed
   ConditionalMinMaxColumn = 'Parameter.Name',
   ConditionalMinMaxValues = c('DO','pH','chfyla','PPFD1','PPFD'),
   ConditionalMin = c(0,0,0,0,0), ConditionalMax = c(30,15,1000,2000,2000)
   #conditional min max filter based on parameter with minimum reasonable value
   #for oxygen, pH, chlorophyll a, and PPFD being 0 and the maximum
   #reasonable value for oxygen and pH being 15 and chlorophyll 1000 and PPFD being 2000
   max.gap = 900)
   #The maximum data gap that should be interpolated is 15 minutes or 900 seconds.

#Example: Running full despiking work flow with batch process from a folder of
#csv files, with default despiking algorithm (threshold of 3 MAD from the median)
dspk.DespikingWorkflow.CSVfileBatchProcess(
   input.directory = 'data/PPFD.data', #Load csv files from the folder 'PPFD.data'
   sep = ',', dec = '.', header = T,  #The csv files are separated by commas with
   #point decimals and the first line is the column names.
   Value = 3, val.NAvalue = -777, #The values are found in the third column.
   #-777 stands for no-value
   DateTime = "DateTime",
   #The datetime column name is "DateTime".
   datetime.format = "\%Y-\%m-\%d \%H:\%M:\%S",
   #The datetimes are character strings with this format "2018-04-23 15:32:18".
   datetime.timezone = 'Etc/GMT-1', #The time zone is UTC+1.
   Min=(-50), Max=1700, #The minimum reasonable value is -50 and the maximum  is 1700
   sampling.interval = NULL,
   #Sampling interval is regular and it will be calculated from the provided data
   precision = NULL, #The data precision will be calculated from the data.
   max.gap = 3600)
   #The maximum data gap that should be interpolated is on hour or 3600 seconds.

#Example: Max filter on a vector with interpolation of resulting data gaps of up to 2 records
example.data = c(2,2,4,16,-4,2,0,96,8,12,26,66,2)
dspk.DespikingWorkflow.CSVfileBatchProcess(
   steps = c(1,3), #run step 1 min/max filter and step 3 data gap interpolation
   Value = example.data, #The values are found in the vector 'example.data'
   Max=10, #Filter out all values above 10
   sampling.interval = 60,
   #This is extraneous information since no time data was given, it will be ignored
   precision = 2, #The data are all even so precision is set to 2.
   max.gap = 2)  #The maximum data gap that should be interpolated is 2 missing values.
>Output CSV file:
>"Value","dspk.Values","dspk.DateTimeNum","dspk.StateOfValue"
>  2,         2,                1,                110
>  2,         2,                2,                110
>  4,         4,                3,                110
>  16,        0,                4,                91
>  -4,       -4,                5,                110
>  2,         2,                6,                110
>  0,         0,                7,                110
>  96,        4,                8,                91
>  8,         8,                9,                110
>  12,        NA,               10,               91
>  26,        NA,               11,               91
>  66,        NA,               12,               91
>  2,         2,                13,               110

#Example: Full despiking work flow on an R dataframe with no data gaps.
#999 is the NA value. Despiking is done with a threshold of 4 using the
#standard deviations from the mean.
dspk.DespikingWorkflow.CSVfileBatchProcess(
     Value = datatable$values, val.NAvalue = 999,
     #The values are found in datatable$values. 999 stands for no-value
     Min=(0), Max=200, #The minimum resonable value is 0 and the maximum  is 200
     despike.threshold = 4, despike.Method = "mean",
     #The despiking algorithm is all data points more than 4 standard deviations
     #from the mean of the surrounding 10 data points
     precision = 0.01, #The data has a precision to the hundredth decimal place.
     max.gap = 10) #the maximum data gap that should be interpolated is 10 samples.

#Example: The same example as above but now entering in the dataframe in
#the Data =' argument.
dspk.DespikingWorkflow.CSVfileBatchProcess(
     Data = datatable, Value = 'values', val.NAvalue = 999,
     #The values are found in datatable$values. 999 stands for no-value
     Min=(0), Max=200, #The minimum reasonable value is 0 and the maximum is 200
     despike.threshold = 4, despike.Method = "mean",
     #The despiking algorithm is all data points more than 4 standard deviations
     #from the mean of the surrounding 10 data points
     max.gap = 10)  #the maximum data gap that should be interpolated is 10 samples.

#Example: Full despiking work flow with a conditional min max filter. No Min or Max
#was given so if the conditions are not met, then the min and max will be set to
#infinity.
dspk.DespikingWorkflow.CSVfileBatchProcess(
     Data = datatable, Value = "values", #The values are found in datatable$values.
     ConditionalMinMaxColumn = "Parameter.Name",
     #The factors for basing the conditional min max are in column "Parameter.Name"
     ConditionalMinMaxValues = c('PercentO2','Temp'),
     ConditionalMin = c(0,-30), ConditionalMax = c(100,150)
     #conditional min max filter based on parameter with minimum reasonable value
     #for percent oxygen being 0\% and for temperature being -30C and maximum
     #reasonable value for percent oxygen being 100\% and for temperature being 150C
     max.gap = 10) #The maximum data gap that should be interpolated is 10 samples.

#Example: Full despiking work flow with a conditional min max filter. A Min and a
#Max was now given so if the conditions are not met, then the min and max will be
#set to 10 and 100. If you give a Min and a Max in addition to the conditional
#values, then if the conditions are not met the min and the max will be set to
#those values given.
dspk.DespikingWorkflow.CSVfileBatchProcess(
     Data = datatable, Value = "values", #The values are found in datatable$values.
     Min = 10, Max = 100, #if the bellow conditional min max values are not met,
     #then it will take these values as the min and max
     ConditionalMinMaxColumn = "Parameter.Name",
     #The factors for basing the conditional min max are in column "Parameter.Name"
     ConditionalMinMaxValues = c('PercentO2','Temp'),
     ConditionalMin = c(0,-30),
     ConditionalMax = c(100,150)
     #conditional min max filter based on parameter with minimum reasonable value
     #for percent oxygen being 0\% and for temperature being -30C and maximum
     #reasonable value for percent oxygen being 100\% and for temperature being 150C
     max.gap = 10) #The maximum data gap that should be interpolated is 10 samples.
}
