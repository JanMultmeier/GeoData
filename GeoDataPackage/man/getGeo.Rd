% Generated by roxygen2 (4.1.0.9001): do not edit by hand
% Please edit documentation in R/GetGeo.R
\name{getGeo}
\alias{getGeo}
\title{Geocodes a geographical location and returns it latitude and longitude}
\usage{
getGeo(addr, source = "google", local = FALSE, country = "Germany")
}
\arguments{
\item{addr}{character vector containing the location to geocode}

\item{source}{either "google" (default) to retriece data from the google maps API, "dstk" for datasciencetoolkit.org, or "arcgis" to use ArcGis by Esri}

\item{local}{boolean. If TRUE, retrieves values from local machine when using dstk as a source, FALSE connects to datasciencetoolkit.org (default)}

\item{country}{needed if source="dstk" to avoid empty results. Defaults to "Germany".}
}
\value{
a data.frame with columns lat and lon
}
\description{
Geocodes a geographical location and returns it latitude and longitude
}
\examples{
getGeo("Weststr. 88, 33615 Bielefeld")
getGeo("Berlin, Alexanderplatz")
getGeo("4308 Lookout Rd, 23455 Virginia Beach, VA",source="dstk",country="USA")
}
\keyword{API}
\keyword{Google}
\keyword{address}
\keyword{arcgis}
\keyword{dstk}
\keyword{geocode}
\keyword{geodata}
\keyword{location}

