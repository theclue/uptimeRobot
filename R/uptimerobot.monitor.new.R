#' @rdname uptimerobot.monitor.new
#' @export
#'
#' @title 
#' Add a new monitor
#'
#' @description
#' \code{uptimerobot.monitor.new} creates a new monitor with the given properties.
#' 
#' @details
#' The function returns the ID of the newly created monitor in case success. An error is thrown otherwise.
#' 
#' The alert contacts are whom to be notified when the monitor goes up/down.
#' 
#' Multiple alert contact IDs can be sent in a character vector or in a data frame. If you pass alert contact IDs in a vector, each element must be formatted in the form \code{<id>_<threshold>_<recurrence>} (note the underscores).
#' If you prefer to format it as a data.frame, it must have these three columns: \code{id, threshold, recurrence}, numeric or integer. Order of the columns doesn't matter.
#' 
#' Please note that thresholds and recurrences can be omitted (default to zero) and, as they are only available in the Pro Plan, they are always 0 in the Free Plan.
#' 
#' @author
#' Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param friendly.name string the friendly (screen) name of the monitor.
#' @param URL string with the URL/IP of the monitor.
#' @param type string or integer with the type of the monitor. You can use both the friendly name (string) or the index (integer) here.
#' @param subtype string used only for "Port monitoring" to set which pre-defined port/service is monitored or if a custom port is monitored. You can use both the friendly name (string) or the index (integer) here.
#' @param port string used only for "Port monitoring" to set the port monitored.
#' @param keyword,type required string in Keyword monitoring".
#' @param keyword.value string with the value of the keyword (required for keyword monitoring).
#' @param HTTP.username string used for password-protected web pages (HTTP Basic Auth). Available for HTTP and keyword monitoring.
#' @param HTTP.password string used for password-protected web pages (HTTP Basic Auth). Available for HTTP and keyword monitoring.
#' @param alert.contacts character vector or data frame with the IDs to alert each with their threshold and recurrence values.
#' @param interval integer with the interval for the monitoring check (in minutes).
#'
uptimerobot.monitor.new <- function(api.key,
                                    friendly.name,
                                    URL,
                                    type,
                                    subtype=NULL,
                                    port=NULL,
                                    interval=5,
                                    keyword.type=NULL,
                                    keyword.value=NULL,
                                    HTTP.username=NULL,
                                    HTTP.password=NULL,
                                    alert.contacts=NULL){
  
  # Decode monitor type
  if(class(type) == "character"){
    type <- as.numeric(factor(tolower(type), labels=1:4, levels=c("http", "keyword", "ping", "port")))
  } else if(!(class(type) %in% c("integer", "numeric"))) stop(paste0(class(type), "is not a valid format for monitor type", sep=" "))
  
  # Decode monitor subtype
  if(!(is.null(subtype))){
    if(class(subtype) == "character"){
      subtype <- as.numeric(factor(toupper(subtype), labels=c(1,2,3,4,5,6,99), levels=c("HTTP", "HTTPS", "FTP", "SMTP", "POP3", "IMPAP", "Custom Port")))
    } else if(!(class(subtype) %in% c("integer", "numeric"))) stop(paste0(class(subtype), "is not a valid format for monitor subtype", sep=" "))
  }
  
  if(!(is.null(alert.contacts))){
    if(is.data.frame(alert.contacts)) {
      
      if(!("threshold" %in% names(alert.contacts))) alert.contacts$threshold <- 0
      if(!("recurrence" %in% names(alert.contacts))) alert.contacts$recurrence <- 0
      
      alert.contacts <- paste(alert.contacts$id, alert.contacts$threshold, alter.contacts$recurrence, sep="_")
    }
    alert.contacts <- paste0(paste(alert.contacts$id, ifelse(is.na(alert.contacts$threshold), 0, alert.contacts$threshold), ifelse(is.na(alert.contacts$recurrence), 0, alert.contacts$recurrence), sep="_"), collapse="-")
    
  }
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/newMonitor?apiKey=",
             api.key,
             "&monitorFriendlyName=", friendly.name,
             "&monitorURL=", URL,
             "&monitorType=", type,
             ifelse(is.null(subtype), "", paste0("&monitorSubType", subtype, sep="=")),
             ifelse(is.null(port), "", paste0("&monitorPort", port, sep="=")),
             "&monitorInterval=", interval,
             ifelse(is.null(keyword.type), "", paste0("&monitorKeywordType", keyword.type, sep="=")),
             ifelse(is.null(keyword.value), "", paste0("&monitorKeywordValue", keyword.value, sep="=")),
             ifelse(is.null(HTTP.username), "", paste0("&monitorHTTPUsername", HTTP.username, sep="=")),
             ifelse(is.null(HTTP.password), "", paste0("&monitorHTTPPassword", HTTP.password, sep="=")),
             ifelse(is.null(alert.contacts), "", paste0("&monitorAlertContacts", alert.contacts, sep="=")),
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
