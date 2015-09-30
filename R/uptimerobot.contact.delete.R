#' Delete an alert contact
#'
#' \code{uptimerobot.contact.delete} removes an alert contanct, unlinking from all the registered monitors.
#' 
#' @return The function returns \code{TRUE} in case success. An error is thrown otherwise.
#'  
#' @author Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param id numeric or integer with the ID of the contact to delete.
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @export 
uptimerobot.contact.delete <- function(api.key, id){
  
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/deleteAlertContact?apiKey=",
             api.key,
             "&alertContactID=", id,
             "&format=json&noJsonCallback=1"
      )      
    ),
    unexpected.escape="keep"
  )
  
  if(data$stat=="ok") {
    return(TRUE)
  }
  else {
    stop(data$message)
  }
  
}
