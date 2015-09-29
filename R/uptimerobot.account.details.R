#' Get the account details for who is linked to the given API key
#'
#' \code{uptimerobot.account.details} returns a list or a vector with the account details connected to the given api key.
#' 
#' @author Gabriele Baldassarre
#' 
#' @return A list or a vector with the account details.
#'
#' @param api.key a string with a valid key for connecting to UptimeRobors public API.
#' @param unlist logical. Set to \code{TRUE} to unlist the output to a named vector, \code{FALSE} to get a named list.
#'
#' @importFrom RCurl getURL
#' @importFrom rjson fromJSON
#' @export 
uptimerobot.account.details <- function(api.key, unlist = FALSE){
  
  data <- fromJSON(
    getURL(
      paste0("https://api.uptimerobot.com/getAccountDetails?apiKey=",
             api.key,
             "&format=json&noJsonCallback=1"
      )      
    ),
    unexpected.escape="keep"
  )
  
  if(data$stat=="ok") {
    
    if(!unlist) return(lapply(data$account, function(x){ as.integer(x)}))
    
    data.unlisted <- as.integer(unlist(data$account))
    names(data.unlisted) <- names(unlist(data$account))
    return(data.unlisted)
  }
  else {
    stop(data$message)
  }
  
}
