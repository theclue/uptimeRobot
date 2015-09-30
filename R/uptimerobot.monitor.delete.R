#' Delete a monitor
#'
#' \code{uptimerobot.monitor.delete} remove a monitor and all existing statistics of it.
#' 
#' @return
#' The function returns \code{TRUE} in case success. An error is thrown otherwise.
#'  
#' @author Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param id numeric or integer with the ID of the monitor to delete.
#' 
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @export 
uptimerobot.monitor.delete <- function(api.key, id){
  
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/deleteMonitor?apiKey=",
             api.key,
             "&monitorID=", id,
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
