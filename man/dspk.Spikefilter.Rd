% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SpikeRemovalFunctions.R
\name{dspk.Spikefilter}
\alias{dspk.Spikefilter}
\title{Spike removal algorithm}
\usage{
dspk.Spikefilter(
  Data = NULL,
  Value,
  NumDateTime = NULL,
  sampling.interval = NULL,
  State.of.value.data = NULL,
  state.of.value.code = 92,
  good.state.of.value.code = 80,
  default.state.of.value.code = 110,
  NAvalue = NULL,
  threshold = 3,
  precision = NULL,
  Method = "median",
  logoutput = F
)
}
\arguments{
\item{Data}{A dataframe. Leave as NULL if you are entering in vectors directly.}

\item{Value}{A vector, datatable column name as a string or datatable column number as numeric}

\item{NumDateTime}{Numeric datetime. A vector, datatable column name as a string or datatable column number as numeric}

\item{sampling.interval}{Time between samples for use in handling data gaps. May be left as NULL and the function will calculate it.}

\item{State.of.value.data}{A vector, datatable column name as a string or datatable column number as numeric}

\item{state.of.value.code}{Number that deleted values are marked with}

\item{good.state.of.value.code}{Number that checked values are marked with}

\item{default.state.of.value.code}{Number that unchecked values are marked with if no State.of.value.data was provided.}

\item{NAvalue}{Value that is read as NA}

\item{threshold}{Number indicating the threshold for defining a spike. By default it is 3, which corresponds to 3 median absolute deviations or 3 standard deviations.}

\item{precision}{The number of decimals in your dataset. Will be calculated if left as NULL.}

\item{Method}{Character string "median" or “mean” indicating the method to use for the despiking. By default “median”.}

\item{logoutput}{TRUE if you want to have a logged record of what the function did}
}
\value{
returns a datatable with columns dspk.Values and dspk.StateOfValue containing the filtered data.
If logoutput is TRUE, then $data contains the datatable and $logdata contains the info for the log file
}
\description{
Outlier removal function using standard deviation or median absolute deviation thresholds
from the 10 surrounding datapoints.
}
\details{
With the default “Method” median
and the default “threshold” 3: all data points that are more than 3
median absolute deviations away from the median of the 10 surrounding data
points (5 before and 5 after) will be deleted. At least 5 surrounding data points
is required for the sample to be evaluated. The algorithm will not look farther
than 5 sampling intervals before and after the data point, for handling data gaps.
If a “sampling.interval” is not provided then it will be calculated as the mode of
the interval between samples.

Spikes must be greater than 4 times the precision of the data, for example if there
is one decimal place then the spike must be at least 0.4 greater than the median/mean
of the surrounding data. This is a bug fix for when there is very little change in the
values and most of the values are exactly the same, then any change what so ever
will be taken as a spike (e.g. 4,4,4,5,4,4,4 the 5 in this list would be taken as a
spike otherwise).

To keep track of what vales were evaluated and removed the output "dspk.StateOfValue" is generated.
The state of value of the deleted values will be set to “state.of.value.code” (default 92). The
state of value of the values that passed the despike test will be set to
“good.state.of.value.code” (default 80). If the value was unchecked (due to too few point)
then the state of value will be left as is.
If the no "sate.of.value.data" was provided then the state of value for unckecked will be
"default.state.of.value.code" (default 110).
}
\examples{
SomeValues <- c(5,6,2,3,5,66,2,2,3,69,8,2,3,3)
SomeTimes <- c(1,2,3,4,5,6,7,8,9,22,23,24,25,26)
ADataframe <- data.frame(SomeValues,SomeTimes)

#entering in a vector into the function.
dspk.Spikefilter(Value = SomeValues)
#entering a dataframe and column name into the function
dspk.Spikefilter(Data = ADataframe, Value = "SomeValues")
#entering a dataframe and column number into the function
dspk.Spikefilter(Data = ADataframe, Value = 1)

#If you have data gaps then provide sampling times so that the
#function won't compare data that isn't actually next to each
#other in time. The time must be provided as a numeric class.
dspk.Spikefilter(Value = SomeValues, NumDateTime = SomeTimes)


#------------------------------------------------------------------------
# If you enter in no "Method" or "threshold" it will evaluate the
#despiking using the default method of a threshold of 3 median absolute
#deviations from the median.

#Running the despiking using the threshold of 5 median absolute deviations from the median.
dspk.Spikefilter(Value = SomeValues, threshold = 5)
#Running the despiking using the threshold of 5 standard deviations from the mean.
dspk.Spikefilter(Value = SomeValues, Method = "mean", threshold = 5)

}
