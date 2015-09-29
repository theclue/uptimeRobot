#' @rdname uptimerobot.monitor.reset
#' @export
#'
#' @title 
#' Delete a monitor
#'
#' @description
#' \code{uptimerobot.monitor.reset} remove all the statistics and logs associated to a monitor ID.
#' 
#' The function returns \code{TRUE} in case success. An error is thrown otherwise.
#'  
#' @author
#' Gabriele Baldassarre
#' 
#' @param api.key string with a valid key for connecting to Uptimerobot API.
#' @param id numeric or integer with the ID of the monitor to delete.
#' 
uptimerobot.monitor.reset <- function(api.key, id){
  
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/resetMonitor?apiKey=",
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
