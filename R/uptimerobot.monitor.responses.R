#' @rdname uptimerobot.monitor.responses
#' @export
#'
#' @title 
#' Get response times for one or more monitors
#'
#' @description
#' \code{uptimerobot.monitor.responses} returns a dataset with all the response times
#' for the given monitors IDs.
#' 
#' The API uses pagination and returns no more than 50 monitors on each page. Use \code{limit} and \code{offset} to set a different number of
#' monitors to get on each page and to move between pages. Leave default values to get all the data.
#' 
#' @author
#' Gabriele Baldassarre
#' 
#' @seealso \code{\link{uptimerobot.monitors}}, \code{\link{uptimerobot.monitor.logs}}, \code{\link{uptimerobot.monitor.contacts}}
#'
#' @param api.key A valid key for connecting to UptimeRobors public API.
#' 
#' @param monitors vector or comma-delimited string with the IDs of the monitors to get.
#' 
#' @param limit An integer value used for pagination. Defines the max number of records to return in each page. Default and max. is 50.
#' 
#' @param offset An integer value to set the index of the first monitor to get (used for pagination).
#'
uptimerobot.monitor.responses <- function(api.key, 
                                          monitors,
                                          limit=50,
                                          offset=0){

  if(is.null(monitors)){
    return(data.frame(monitor.id=character(),
                      datetime=as.Date(character()), 
                      value=integer(), 
                      stringsAsFactors=FALSE) )
  }
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/getMonitors?apiKey=",
             api.key,
             "&monitors=", paste0(unique(unlist(strsplit(monitors, split = ","))), collapse = "-"),
             "&responseTimes=1",
             "&limit=", limit,
             "&offset=", offset,
             "&format=json&noJsonCallback=1"
      )      
    ),
    unexpected.escape="keep"
  )
  
  if(data$stat=="ok") {
    return((function() {
      data.merged <- do.call(
        rbind.fill,lapply(data$monitors$monitor, function(x) {
          
          response.times <- do.call(rbind, lapply(x$responsetime, function(y){
             y$datetime <- strptime(y$datetime, format="%m/%d/%y %H:%M:%S", tz="GMT")
             do.call(data.frame, list(y, stringsAsFactors = FALSE))
          }))
          response.times$monitor.id <- x$id

          return(response.times[,c(3,1,2)])
        })
      )

      # Convert to proper datatypes
      data.merged$value <- as.integer(data.merged$value)
      
      # Pagination
      if((as.integer(data$offset) + as.integer(data$limit)) >= as.integer(data$total)) return(data.merged)
      else {
        rbind.fill(data.merged, uptimerobot.monitor.responses(api.key = api.key, 
                                                     monitors = monitors, 
                                                     limit = limit,
                                                     offset = as.character(as.integer(offset) + as.integer(limit))))
      }
    })()
    )
  }
  else {
    message(paste("Error:", data$message))
    return(NULL)
  }
  
}
