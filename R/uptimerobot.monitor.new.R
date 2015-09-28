#' @rdname uptimerobot.monitor.new
#' @export
#'
#' @title 
#' Add a new monitor
#'
#' @description
#' \code{uptimerobot.monitor.new} create a new monitor with the given properties.
#' 
#' @details
#' The function returns the ID of the newly created monitor in case success, an error is thrown otherwise.
#' 
#' The alert contacts are whom to be notified when the monitor goes up/down.
#' 
#' Multiple alert contact IDs can be sent in a character vector or in a data frame. If you pass alert contact IDs in a vector, each element must be formatted in the form \code{<id>_<threshold>_<recurrence>} (note the underscores).
#' If you prefer to format it as a data.frame, it must have these three columns: \code{id, threshold, recurrence}, numeric or integer. Order of the columns doesn't matter.
#' 
#' Please note that thresholds and recurrences can be omitted (default to zero) and, as they are only available in the Pro Plan, they are always 0 in the Free Plan.
#'
#'  
#' @author
#' Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param monitor.friendly.name string the friendly (screen) name of the monitor.
#' @param monitor.URL string with the URL/IP of the monitor.
#' @param monitor.type string or integer with the type of the monitor. You can use both the friendly name (string)or the index (integer) here.
#' @param monitor.subtype string used only for "Port monitoring" to set which pre-defined port/service is monitored or if a custom port is monitored. You can use both the friendly name (string) or the index (integer) here.
#' @param monitor.port string used only for "Port monitoring" to set the port monitored.
#' @param monitor.keyword,type required string in Keyword monitoring".
#' @param monitor.keyword.value string with the value of the keyword (required for keyword monitoring).
#' @param monitor.HTTP.username string used for password-protected web pages (HTTP Basic Auth). Available for HTTP and keyword monitoring.
#' @param monitor.HTTP.password string used for password-protected web pages (HTTP Basic Auth). Available for HTTP and keyword monitoring.
#' @param monitor.alert.contacts character vector or data frame with the IDs to alert each with their threshold and recurrence values.
#' @param monitor.interval integer with the interval for the monitoring check (in minutes)
#'
uptimerobot.monitor.new <- function(api.key,
                                    monitor.friendly.name,
                                    monitor.URL,
                                    monitor.type,
                                    monitor.subtype=NA,
                                    monitor.port=NA,
                                    monitor.interval=5,
                                    monitor.keyword.type=NA,
                                    monitor.keyword.value=NA,
                                    monitor.HTTP.username=NA,
                                    monitor.HTTP.password=NA,
                                    monitor.alert.contacts=NA){
  
  # Decode monitor type
  if(class(monitor.type) == "character"){
    monitor.type <- as.numeric(factor(monitor.type, labels=1:4, levels=c("HTTP", "Keyword", "Ping", "Port")))
  } else if(!(class(monitor.type) %in% c("integer", "numeric"))) stop(paste0(class(monitor.type), "is not a valid format monitor.type", sep=" "))
  
  # Decode monitor subtype
  if(class(monitor.subtype) == "character"){
    
    monitor.subtype <- as.numeric(factor(monitor.subtype, labels=c(1,2,3,4,5,6,99), levels=c("HTTP", "HTTPS", "FTP", "SMTP", "POP3", "IMPAP", "Custom Port")))

    } else if(!(class(monitor.type) %in% c("integer", "numeric"))) stop(paste0(class(monitor.type), "is not a valid format monitor.type", sep=" "))
  
  
  if(!(is.na(monitor.alert.contacts))){
    if(is.data.frame(monitor.alert.contacts)) {
      
      if(!("threshold" %in% names(monitor.alert.contacts))) monitor.alert.contacts$threshold <- 0
      if(!("recurrence" %in% names(monitor.alert.contacts))) monitor.alert.contacts$recurrence <- 0
      
      monitor.alert.contacts <- paste(monitor.alert.contacts$id, monitor.alert.contacts$threshold, monitor.alter.contacts$recurrence, sep="_")
    }
    monitor.alert.contacts <- paste0(paste(monitor.alert.contacts$id, ifelse(is.na(monitor.alert.contacts$threshold), 0, monitor.alert.contacts$threshold), ifelse(is.na(monitor.alert.contacts$recurrence), 0, monitor.alert.contacts$recurrence), sep="_"), collapse="-")
    
  }
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/newMonitor?apiKey=",
             api.key,
             "&monitorFriendlyName=", monitor.friendly.name,
             "&monitorURL=", monitor.URL,
             "&monitorType=", monitor.type,
             ifelse(is.na(monitor.subtype), "", paste0("&monitorSubType", monitor.subtype, sep="=")),
             ifelse(is.na(monitor.subtype), "", paste0("&monitorPort", monitor.port, sep="=")),
             "&monitorInterval=", monitor.interval,
             ifelse(is.na(monitor.keyword.type), "", paste0("&monitorKeywordType", monitor.keyword.type, sep="=")),
             ifelse(is.na(monitor.keyword.value), "", paste0("&monitorKeywordValue", monitor.keyword.value, sep="=")),
             ifelse(is.na(monitor.HTTP.username), "", paste0("&monitorHTTPUsername", monitor.HTTP.username, sep="=")),
             ifelse(is.na(monitor.HTTP.password), "", paste0("&monitorHTTPPassword", monitor.HTTP.password, sep="=")),
             ifelse(is.na(monitor.alert.contacts), "", paste0("&monitorAlertContacts", monitor.alert.contacts, sep="=")),
             "&format=json&noJsonCallback=1"
      )      
    ),
    unexpected.escape="keep"
  )
  
  if(data$stat=="ok") {
    return(as.numeric(data$monitor$id))
  }
  else {
    stop(data$message)
  }
  
}
