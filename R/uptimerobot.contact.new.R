#' Add a new alert contact
#'
#' \code{uptimerobot.contact.new} creates a new alert contact with the given properties.
#' 
#' @details
#' The alert contacts are whom to be notified when the monitor goes up/down. 
#' 
#' @return The function returns the ID of the newly created contact in case success. An error is thrown otherwise.
#' 
#' @author Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param type string or integer with the type of the contact. You can use both the friendly name (string) or the index (integer) here.
#' @param value string with the value of the contact (ie. the email address).
#' @param friendly.name string the friendly (screen) name of the contact.
#' 
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @export 
uptimerobot.contact.new <- function(api.key,
                                    type,
                                    value,
                                    friendly.name){
  
  # Decode contact type
  if(!(is.null(type))) {
    if(class(type) == "character"){
      type <- as.numeric(as.character(factor(tolower(type), labels=c("1", "2", "3", "4", "5", "6", "7", "9", "10", "11"), levels=c("sms", "email", "twitter dm", "boxcar", "webhook", "pushbullet", "zapier", "pushover", "hipchat", "slack"))))
    } else if(!(is.na(type)) & !(class(type) %in% c("integer", "numeric"))) stop(paste(class(type), "cannot be coerced to express a contact type", sep=" "))
  }
  
  if(is.null(type) | is.na(type)) stop("contact type missing or not recognized.")
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/newAlertContact?apiKey=",
             api.key,
             "&alertContactFriendlyName=", friendly.name,
             "&alertContactType=", type,
             "&alertContactValue=", value,
             "&format=json&noJsonCallback=1"
      )      
    ),
    unexpected.escape="keep"
  )
  
  if(data$stat=="ok") {
    return(as.numeric(data$contact$id))
  }
  else {
    stop(data$message)
  }  
}
