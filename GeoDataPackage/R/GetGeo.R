#' Geocodes a geographical location and returns it latitude and longitude
#' 
#' @param addr character vector containing the location to geocode
#' @param source either "google" (default) to retriece data from the google maps API, "dstk" for datasciencetoolkit.org, or "arcgis" to use ArcGis by Esri
#' @param local boolean. If TRUE, retrieves values from local machine when using dstk as a source, FALSE connects to datasciencetoolkit.org (default)
#' @param country needed if source="dstk" to avoid empty results. Defaults to "Germany".
#' @return a data.frame with columns lat and lon
#' @keywords Google dstk arcgis API geodata geocode location address
#' @export
#' @examples
#' getGeo("Weststr. 88, 33615 Bielefeld")
#' getGeo("Berlin, Alexanderplatz")
#' getGeo("4308 Lookout Rd, 23455 Virginia Beach, VA",source="dstk",country="USA")

getGeo <- function (addr, source = "google", local = FALSE, country = "Germany")
{
  if ((source %in% c("google", "dstk", "arcgis")) == FALSE) {
    stop("Unbekanntes 'source'-Argument: google, dstk oder arcgis sind erlaubt")
  }
  require(rjson)
  results <- data.frame(lat = numeric(length = length(addr)),
                        lon = numeric(length = length(addr)))
  if (source == "google") {
    for (i in seq_along(addr)) {
      success <- FALSE
      attempts <- 0
      while (success == FALSE & attempts < 3) {
        attempts <- attempts + 1
        url <- paste0("http://maps.googleapis.com/maps/api/geocode/json?address=",
                      gsub(" ", "+", addr[i]), "&sensor=false")
        data <- fromJSON(file = url)
        if (data$status == "OK") {
          results$lat[i] <- data$results[[1]]$geometry$location$lat
          results$lon[i] <- data$results[[1]]$geometry$location$lng
          success <- TRUE
        }
        if (data$status == "ZERO_RESULTS") {
          warning("Keine Ergebnisse gefunden!")
          results$lat[i] <- NA
          results$lon[i] <- NA
          success <- TRUE
        }
        if (data$status == "OVER_QUERY_LIMIT") {
          Sys.sleep(2)
        }
        if (attempts == 3) {
          stop("Abfragelimit erreicht")
        }
      }
    }
  }
  if (source == "dstk") {
    ifelse(local == TRUE, api_base <- "http://localhost:8080",
           api_base <- "http://www.datasciencetoolkit.org")
    if (length(country != length(addr))) {
      country <- rep(country, len = length(addr))
    }
    for (i in seq_along(addr)) {
      success <- FALSE
      attempts <- 0
      while (success == FALSE & attempts < 100) {
        attempts <- attempts + 1
        url <- paste0(api_base, "/maps/api/geocode/json?sensor=false&address=",
                      gsub(" ", "+", addr[i]), ",+", country[i])
        data <- fromJSON(file = url)
        if (data$status == "OK") {
          results$lat[i] <- data$results[[1]]$geometry$location$lat
          results$lon[i] <- data$results[[1]]$geometry$location$lng
          success <- TRUE
        }
        if (attempts == 100) {
          results$lat[i] <- NA
          results$lon[i] <- NA
          success <- TRUE
        }
        if (data$status == "ZERO_RESULTS") {
          warning("Keine Ergebnisse gefunden!")
          results$lat[i] <- NA
          results$lon[i] <- NA
          success <- TRUE
        }
        if (attempts == 100) {
          warning("Maximale Iterationen erreicht")
          results$lat[i] <- NA
          results$lon[i] <- NA
        }
      }
    }
  }
  if (source == "arcgis") {
    for (i in seq_along(addr)) {
      url <- paste0("http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/find?text=",
                    gsub(" ", "+", addr[i]), "&f=json")
      data <- fromJSON(file = url)
      if (length(data$locations)==0) {results$lat[i] <- NA
                                      results$lon[i] <- NA
      }
      if (length(data$locations)>0) {results$lat[i] <- data$locations[[1]]$feature$geometry$y
                                     results$lon[i] <- data$locations[[1]]$feature$geometry$x
      }     
    }
  }
  return(results)
}