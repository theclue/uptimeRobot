#' @rdname uptimerobot.contacts
#' @export
#'
#' @title 
#' Get general informations about the registered contacts
#'
#' @description
#' \code{uptimerobot.contacts} return a dataset with general informations
#' for a set of contacts used to be alert in case of uptimerobot events.
#' 
#' @details 
#' If a vector of contact IDs is not given, the function will return data for all the available contacts.
#' 
#' The API uses pagination and returns no more than 50 contacts on each page. Use \code{limit} and \code{offset} to set a different number of
#' monitors to get on each page and to move between pages. Leave default values to get all the data.
#' 
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{uptimerobot.monitors}}
#'
#' @param api.key A valid key for connecting to UptimeRobors public API.
#' 
#' @param contacts vector or comma-delimited string with the IDs of the contacts to get.
#' If the argument is missing or NA, all the available monitors will be returned. 
#'
#' @param fields vector or comma-delimited string with the general informations to include in the output dataset.
#' 
#' @param limit An integer value used for pagination. Defines the max number of records to return in each page. Default and max. is 50.
#' 
#' @param offset An integer value to set the index of the first monitor to get (used for pagination).
#'
uptimerobot.contacts <- function(api.key, 
                                  contacts=NA,
                                  limit=50,
                                  offset=0,
                                  fields="id,value,friendlyname,type,status"){
  
  fields <- ifelse(length(fields)==0, "id", fields)
  
  fields.o <- unique(unlist(strsplit(fields, split = ",")))
  fields.v <- unique(c(unlist(strsplit(fields, split = ","))), "id")

  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/getAlertContacts?apiKey=",
             api.key,
             ifelse(is.na(contacts), "", paste0("&alertcontacts=", paste0(unique(unlist(strsplit(contacts, split = ","))), collapse = "-"), sep="")),
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
        rbind.fill,lapply(data$alertcontacts$alertcontact, function(x) {
          return(do.call(data.frame, list(x[which(names(x) %in%  fields.v)], stringsAsFactors = FALSE)))
        })
      )
      
      # Convert to proper datatypes
      if("type" %in% fields.o) data.merged$type <- factor(as.integer(data.merged$type), levels=c(1,2,3,4,5,6,7,9,10,11), labels=c("SMS", "E-mail", "Twitter DM", "Boxcar", "Web-Hook", "Pushbullet", "Zapier", "Pushover", "Hipchat", "Slack"))
      if("status" %in% fields.o) data.merged$status <- factor(as.integer(data.merged$status), levels=c(0, 1, 2), labels=c("not activated", "paused", "active"))
      
      if(!("id" %in% fields.o)) data.merged$id <- NULL
      
      # Pagination
      if((as.integer(data$offset) + as.integer(data$limit)) >= as.integer(data$total)) return(data.merged)
      else {
        rbind.fill(data.merged, uptimerobot.contacts(api.key = api.key, 
                                                     contacts = contacts, 
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
