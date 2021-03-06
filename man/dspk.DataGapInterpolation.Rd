% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpikeRemovalFunctions.R
\name{dspk.DataGapInterpolation}
\alias{dspk.DataGapInterpolation}
\title{Linear interpolation}
\usage{
dspk.DataGapInterpolation(
  Data = NULL,
  Value,
  precision = NULL,
  NumDateTime = NULL,
  max.gap = Inf,
  State.of.value.data = NULL,
  state.of.value.code = 93,
  default.state.of.value.code = 110,
  NAvalue = NULL,
  logoutput = F
)
}
\arguments{
\item{Data}{A dataframe. Leave as NULL if you are entering in vectors.}

\item{Value}{A vector, datatable column name as a string or datatable column number as numeric}

\item{precision}{If you enter in a precision then it will round the interpolated values. Precision is the is the smallest measurable unit on the scale. e.g. 13000 would have precision = 1000 and 0.23 would have precision = 0.01}

\item{NumDateTime}{A Numeric datetime. Vector, datatable column name as a string or datatable column number as numeric}

\item{max.gap}{is the largest data gap that you want to perform linear interpolation on.}

\item{State.of.value.data}{A vector, datatable column name as a string or datatable column number as numeric}

\item{state.of.value.code}{A number that lables all values that were interpolated}

\item{default.state.of.value.code}{Number that values are marked with if no State.of.value.data was provided.}

\item{NAvalue}{This values is read as NA}

\item{logoutput}{TRUE if you want to have a logged record of what the function did}
}
\value{
returns a datatable with columns dspk.Values and dspk.StateOfValue containing the interpolated data.
If logoutput is TRUE, then $data contains the datatable and $logdata contains the info for the log file
}
\description{
Linear interpolation between gaps. Excludes gaps greater than "max.gap". If no "NumDateTime" is provided
then max.gap is in the unit samplings. If a "NumDateTime" is provided then max.gap must be in the same unit.
The entered in "NumDateTime" must be numeric class.
}
\examples{
SomeValues <- c(5,NA,NA,NA,5,3,2,2,3,NA,8,2,3,3)
SomeTimes <- c(1,2,3,4,5,6,7,8,9,22,23,24,25,26)
ADataframe <- data.frame(SomeValues,SomeTimes)

#entering in a vector into the function.
dspk.DataGapInterpolation(Value = SomeValues)
#entering a dataframe and column name into the function
dspk.DataGapInterpolation(Data = ADataframe, Value = "SomeValues")
#entering a dataframe and column number into the function
dspk.DataGapInterpolation(Data = ADataframe, Value = 1)

#If you don't want gaps larger than 2 samplings to be interpolated
dspk.DataGapInterpolation(Value = SomeValues, max.gap = 2)

#If you have data gaps then provide sampling times so that the
#function will take the sampling times into account when assesing max.gap.
#The time must be provided as a numeric class and max.gap must be in the
#same unit as your provided time.
dspk.DataGapInterpolation(Value = SomeValues, NumDateTime = SomeTimes, max.gap = 2)
}
