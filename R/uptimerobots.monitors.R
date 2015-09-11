#' @rdname uptimerobots.monitors
#' @export
#'
#' @title 
#' Extract monitors metadata and response times.
#'
#' @description
#' \code{uptimerobots.monitors} get the available response times and metadata of the given \code{monitors} vectors.
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{uptimerobots.monitors}}
#'
#' @param monitors vector or comma-delimited string with the IDs of the monitors to get. If the argument is missing, all monitors will be get. 
#'
#' @param api.key A valid key for connecting to UptimeRobors public API.
#'
#' @param response.times Set to TRUE to get the response times with the metadata. FALSE to get  metadata values only.
#'
#' @param fields vector or comma-delimited string with the columns of metadata to get.
#' \code{n}.
#'
uptimerobots.monitors <- function(api.key, monitors=NA, response.times=FALSE, fields="id,friendlyname,url,type,port,interval,status"){
  
  fields <- ifelse(length(fields)==0, "id", fields)
  
  fields.o <- unique(unlist(strsplit(fields, split = ",")))
  fields.v <- unique(c(unlist(strsplit(fields, split = ","))), "id")
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/getMonitors?apiKey=",
             api.key,
             ifelse(is.na(monitors), "", paste0("&monitors=", paste0(unique(unlist(strsplit(monitors, split = ","))), collapse = "-"), sep="")),
             "&responseTimes=", as.integer(response.times),
             "&format=json&noJsonCallback=1"
      )      
    ),
    unexpected.escape="keep"
  )
  
  if(data$stat=="ok") {
    return((function() {
      data.merged <- do.call(
        rbind.fill,lapply(data$monitors$monitor, function(x) {
          header <- do.call(data.frame, list(x[which(names(x) %in%  fields.v)], stringsAsFactors = FALSE))
          
          if(!response.times) return(header)
          
          response.times <- do.call(rbind, lapply(x$responsetime, function(y){
            do.call(data.frame, list(y, stringsAsFactors = FALSE))
          }))
          response.times$id <- header$id
          
          return(merge(header, response.times, by = "id"))
        })
      )
      
      if(!("id" %in% fields.o)) data.merged$id <- NULL
      return(data.merged)
    })()
    )
  }
  else {
    message(data$message)
    return(NULL)
  }
  
}
