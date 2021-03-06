% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpikeRemovalFunctions.R
\name{dspk.MinMaxfilter}
\alias{dspk.MinMaxfilter}
\title{Min max filter}
\usage{
dspk.MinMaxfilter(
  Data = NULL,
  Value,
  Min,
  Max,
  State.of.value.data = NULL,
  state.of.value.code = 91,
  default.state.of.value.code = 110,
  NAvalue = NULL,
  logoutput = F
)
}
\arguments{
\item{Data}{A datatable or NULL}

\item{Value}{A vector, datatable column name as a string or datatable column number as numeric}

\item{Min}{Minimum value to keep}

\item{Max}{Maximum value to keep}

\item{State.of.value.data}{A vector, datatable column name as a string or datatable column number as numeric}

\item{state.of.value.code}{number that deleted values will be taged with}

\item{default.state.of.value.code}{number that values will be tagged with if no State.of.value.data is given}

\item{NAvalue}{Value that should be read as NA}

\item{logoutput}{TRUE if you want to have a logged record of what the function did}
}
\value{
returns a datatable with columns dspk.Values and dspk.StateOfValue containing the filtered data.
If logoutput is TRUE, then $data contains the datatable and $logdata contains the info for the log file
}
\description{
Mininum maximum filter.
}
\examples{
SomeValues <- c(5,NA,NA,NA,5,3,2,2,3,NA,8,2,3,3)
SomeTimes <- c(1,2,3,4,5,6,7,8,9,22,23,24,25,26)
ADataframe <- data.frame(SomeValues,SomeTimes)

#entering in a vector into the function.
dspk.MinMaxfilter(Value = SomeValues, Min = 3,Max = 5)
#entering a dataframe and column name into the function
dspk.MinMaxfilter(Data = ADataframe, Value = "SomeValues", Min = 3,Max = 5)
#entering a dataframe and column number into the function
dspk.MinMaxfilter(Data = ADataframe, Value = 1, Min = 3,Max = 5)
}
