#' @rdname uptimerobot.monitor.delete
#' @export
#'
#' @title 
#' Delete a monitor
#'
#' @description
#' \code{uptimerobot.monitor.delete} remove a monitor and all existing statistics of it.
#'  
#' @author
#' Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param id numeric or integer with the ID of the monitor to delete.
#' 
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
