% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/GetDist.R
\name{getDist}
\alias{getDist}
\title{Retrieves a route between start and destination from Google Maps and extracts time or distance}
\usage{
getDist(from, to, modus = "driving", get = "distance")
}
\arguments{
\item{from}{character vector containing an starting address or location}

\item{to}{character vector containing an destination address or location}

\item{modus}{a character vector indicating the mode of transportation, either "driving", "walking" or "bicycling". Defaults to "driving".}

\item{get}{a character vector indicating what to retrieve, "distance" or "duration". Defaults to "distance".}
}
\value{
a numeric vector with the distance in km, rounded to 1 decimal spaces, or the duration of the trip in h, rounded to two decimal spaces.
}
\description{
Retrieves a route between start and destination from Google Maps and extracts time or distance
}
\examples{
getDist("Bielefeld","Berlin")
getDist("Friedelstr. 10, 12047 Berlin","Weststr. 88, 33615 Bielefeld",modus="walking",get="duration")
}
\keyword{API}
\keyword{Google}
\keyword{distance}
\keyword{duration}
\keyword{geodata}

