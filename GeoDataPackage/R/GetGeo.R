#' Geocodes a geographical location and returns it latitude and longitude
#' 
#' @param addr character vector containing the location to geocode
#' @param source either "google" (default) to retriece data from the google maps API, or "dstk" for datasciencetoolkit.org
#' @param country needed if source="dstk" to avoid empty results. Defaults to "Germany".
#' @return a data.frame with lat and lon as columns
#' @keywords Google dstk API geodata geocode location
#' @export
#' @examples
#' getGeo("Weststr. 88, 33615 Bielefeld")
#' getGeo("Berlin, Alexanderplatz")
#' getGeo("4308 Lookout Rd, 23455 Virginia Beach, VA",source="dstk",country="USA")

getGeo <- function(addr,source="google",country="Germany") {
  require(rjson)
  cntry <- rep(country, times=length(addr))
  results <- data.frame("lat"=numeric(length=length(addr)),"lon"=numeric(length=length(addr)))
    for (i in seq_along(addr)) {
      if (source=="google") {
        url <- paste0("http://maps.googleapis.com/maps/api/geocode/json?address=",gsub(" ","+",addr[i]),"&sensor=false")
      }
      if (source=="dstk") {
        url <- paste0("http://www.datasciencetoolkit.org/maps/api/geocode/json?sensor=false&address=",gsub(" ","+",addr[i]),",+",cntry[i])
      }
    data <- fromJSON(file=url)
    if (data$status=="ZERO_RESULTS") {
      warning("Keine Ergebnisse gefunden!")
    }
    if (data$status=="OVER_QUERY_LIMIT") {
      stop("Abfragelimit erreicht")
    }
    if (data$status=="OK") {
      results$lat[i] <- data$results[[1]]$geometry$location$lat
      results$lon[i] <- data$results[[1]]$geometry$location$lng
    }
    else {
      results$lat[i] <- NA
      results$lon[i] <- NA
    }
  }
return(results)
}