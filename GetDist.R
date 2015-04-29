#This function retrieves a route between start and destination from Google Maps and extracts time or distance

getDist <- function(from,to,modus="driving",get="distance") {
  library(rjson)
  metric <- numeric(length=length(from))
  for (i in seq_along(from)) {
    url <- paste0("http://maps.googleapis.com/maps/api/distancematrix/json?origins=Deutschland+",gsub(" ","+",from[i]),"&destinations=Deutschland+",gsub(" ","+",to[i]),"&mode=",modus,"&units=metric&language=de-DE&sensor=false")
    data <- fromJSON(file=url)
    
    if (data$status=="INVALID_REQUEST") {
      warning("Ungültige Anfrage")
      metric[i] <- NA
    }
    
    if (data$status=="IUNKNOWN_ERROR") {
      warning("Unbekannter Fehler - erneut probieren!")
      metric[i] <- NA
    }
    
    if (data$status=="MAX_ELEMENTS_EXCEEDED") {
      stop("Maximale Anzahl von Elementen pro Anfrage überschritten")
      metric[i] <- NA
    }

    if (data$status=="OVER_QUERY_LIMIT") {
      stop("Maximale Anzahl von Anfragen überschritten")
      metric[i] <- NA
    }

    if (data$status=="REQUEST_DENIED") {
      stop("Anfrage abgelehnt")
      metric[i] <- NA
    }
    
    if (data$rows[[1]]$elements[[1]]$status=="NOT_FOUND") {
      warning("Start- und/oder Zielort nicht gefunden")
      metric[i] <- NA
    }
    
    if (data$rows[[1]]$elements[[1]]$status=="ZERO_RESULTS") {
      warning("Keine Route zwischen Start- und Zielort gefunden")
      metric[i] <- NA
    }
    
    if (data$status=="OK" & data$rows[[1]]$elements[[1]]$status=="OK") {

      if (get=="distance") {
        metric[i] <- data$rows[[1]]$elements[[1]]$distance$value
        metric[i] <- round(metric[i]/1000,digits=1)
      }
      
      if(get=="duration") {
        metric[i] <- data$rows[[1]]$elements[[1]]$duration$value
        metric[i] <- round(metric[i]/(60^2),digits=2)
      }
    
    }
    
  }
  return(metric)
}