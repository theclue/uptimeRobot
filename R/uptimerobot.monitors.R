#' @rdname uptimerobot.monitors
#' @export
#'
#' @title 
#' Get general informations for one or more monitors
#'
#' @description
#' \code{uptimerobots.monitors.responses} return a dataset with general informations
#' for a set of monitors.
#' 
#' @details 
#' If a vector of monitor is not given, the function will return data for all the available monitors.
#' 
#' \code{summary} parameter expect a lists of three named logic values that set which columns of additional statistics for each monitor must  be added to output dataset for
#' each available monitor: 
#' 
#' \enumerate{
#'   \item \code{response.times} set to \code{TRUE} to add the number of pings with response times available for the monitor. These values can be queried using \code{\link{uptimerobot.monitor.responses}} function.
#'   \item \code{log.records} set to \code{TRUE} to add the number of log entries recorded for the monitor. These records can be queried using \code{\link{uptimerobot.monitor.logs}} function.
#'   \item \code{alert.contacts} set to \code{TRUE} to add number of alert contacts binded to the monitor. Detailed informations about these contacts can be queried using \code{\link{uptimerobot.monitor.contacts}} function.
#' }
#' 
#' You may just add the elements you want to include into the list, as they default to \code{FALSE} if missing. Set an empty list to exclude all the summary statistics from the output.
#'
#' The API uses pagination and returns no more than 50 monitors on each page. Use \code{limit} and \code{offset} to set a different number of
#' monitors to get on each page and to move between pages. Leave default values to get all the data.
#'
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{uptimerobot.monitor.responses}}, \code{\link{uptimerobot.monitor.logs}}, \code{\link{uptimerobot.monitor.contacts}}
#'
#' @param api.key A valid key for connecting to UptimeRobors public API.
#' 
#' @param monitors vector or comma-delimited string with the IDs of the monitors to get.
#' If the argument is missing or NA, all the available monitors will be returned. 
#'
#' @param summary list of logical values to flag summary indicators to add to the output dataset.
#' 
#' @param types vector or comma-delimited string of monitor types. If not \code{NA}, the function will return all monitors types (HTTP, keyword, ping..) in an account. Else, it is possible to define any number of monitor types.
#' 
#' @param statutes vector or comma-delimited string of monitor statutes. If not \code{NA}, the function will return  all monitors statuses (up, down, paused) in an account. Else, it is possible to define any number of monitor statuses.
#'
#' @param search An optional keyword of to search within monitor.url and monitor.friendly.name to get filtered results.
#'        
#' @param limit An integer value used for pagination. Defines the max number of records to return in each page. Default and max. is 50.
#' 
#' @param offset An integer value to set the index of the first monitor to get (used for pagination).
#'
#' @param fields vector or comma-delimited string with the general informations to include in the output dataset.
#' \code{n}.
#'
uptimerobot.monitors <- function(api.key, 
                                 monitors=NA,
                                 types=NA,
                                 statuses=NA,
                                 search=NA,
                                 summary=list(),
                                 limit=50,
                                 offset=0,
                                 fields="id,friendlyname,url,type,port,interval,status"){
  
  fields <- ifelse(length(fields)==0, "id", fields)
  
  fields.o <- unique(unlist(strsplit(fields, split = ",")))
  fields.v <- unique(c(unlist(strsplit(fields, split = ","))), "id")
  
  include.stat <- function(element){
    if(element %in% names(summary)){
      return(as.logical(summary[element]))
    } else return(FALSE)
  }
  include.responses <- include.stat("response.times")
  include.logs <- include.stat("log.records")
  include.contacts <- include.stat("alert.contacts")
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/getMonitors?apiKey=",
             api.key,
             ifelse(is.na(monitors), "", paste0("&monitors=", paste0(unique(unlist(strsplit(monitors, split = ","))), collapse = "-"), sep="")),
             ifelse(is.na(types), "", paste0("&types=", paste0(unique(unlist(strsplit(types, split = ","))), collapse = "-"), sep="")),
             ifelse(is.na(statuses), "", paste0("&statutes=", paste0(unique(unlist(strsplit(statutes, split = ","))), collapse = "-"), sep="")),
             ifelse(is.na(search), "", paste0("&search=", search, sep="")),
             "&responseTimes=", as.integer(include.responses),
             "&logs=", as.integer(include.logs),
             "&showMonitorAlertContacts=", as.integer(include.contacts),
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
          header <- do.call(data.frame, list(x[which(names(x) %in%  fields.v)], stringsAsFactors = FALSE))
          
          # Baking out the dataset
          if(include.responses) header$responses <- length(x$responsetime)
          if(include.logs) header$logs <- length(x$log)
          if(include.contacts) header$contacts <- length(x$alertcontact)
          
          return(header)
        })
      )
      
      # Convert to proper datatypes
      if("port" %in% fields.o) data.merged$port <- as.integer(data.merged$port)
      if("interval" %in% fields.o) data.merged$interval <- as.integer(data.merged$interval)
      
      # Lookup factors
      if("type" %in% fields.o) data.merged$type <- factor(as.integer(data.merged$type), levels=1:4, labels=c("HTTP(s)", "Keyword", "Ping", "Port"))
      if("status" %in% fields.o) data.merged$status <- factor(as.integer(data.merged$status), levels=c(0, 1, 2, 8, 9), labels=c("paused", "not checked yet", "up", "seems down", "down"))
      if("subtype" %in% fields.o) data.merged$subtype <- factor(as.integer(data.merged$subtype), levels=c(1,2,3,4,5,6,99), labels=c("HTTP (80)", "HTTPS (443)", "FTP (21)", "SMTP (25)", "POP3 (110)", "IMPAP (143)", "Custom Port"))
      if("keywordtype" %in% fields.o) data.merged$keywordtype <- factor(as.integer(data.merged$keywordtype), levels=c(1,2), labels=c("exists", "not exists"))
      

      if(!("id" %in% fields.o)) data.merged$id <- NULL
      
      # Pagination
      if((as.integer(data$offset) + as.integer(data$limit)) >= as.integer(data$total)) return(data.merged)
      else {
        rbind.fill(data.merged, uptimerobot.monitors(api.key = api.key, 
                                                     monitors = monitors,
                                                     types = types,
                                                     statutes = statutes,
                                                     summary = summary , 
                                                     limit = limit,
                                                     offset = as.character(as.integer(offset) + as.integer(limit)),
                                                     fields = fields)
        )
      }
    })()
    )
  }
  else {
    message(paste("Error:", data$message))
    return(NULL)
  }
  
}