#' @rdname uptimerobot.fields
#' @export
#'
#' @title 
#' Get list of UptimeRobot API available fields
#'
#' @description
#' \code{uptimerobots.fields} returns a list of vectors of available fields for commodity uses.
#' 
#' @details
#' The function returns a list of three named elements which in turn contains a set of available fields for a given \code{set}:
#' \enumerate{
#'   \item \code{typical} returns a typical set of fields, used in most situations;
#'   \item \code{full} returns the full set of available fields, including passwords and other potentially confidential data;
#'   \item \code{compact} return a minimal set of fields.
#' }
#' 
#' \code{type} parameter is used to choose which set of fields to return in a list of vectors. These sets are available:
#' \code{monitor}, \code{contact}.
#'
#' @author
#' Gabriele Baldassarre
#' @seealso \code{\link{uptimerobot.monitors}}, \code{\link{uptimerobot.contacts}}
#'
#' @param type string with the type of fields to be reported. Only \code{monitor}, \code{contact} are currently supported.
#' 
uptimerobot.fields <- function(type){
  
  if(type == "monitor"){
    return(
      list("typical" = c("id", "friendlyname", "url", "type", "interval", "status"),
           "full" = c("id", "friendlyname", "url", "type", "subtype", "keywordtype", "keywordvalue", "httpusername", "httppassword", "port", "interval", "status"),
           "compact" = c("id", "friendlyname", "status"))
      )
  }
  
  if(type == "contact"){
    return(
      list("typical" = c("id", "type", "value", "friendlyname", "status"),
           "full" = c("id", "type", "value", "friendlyname", "status"),
           "compact" = c("id", "type", "value", "friendlyname"))
    )
  }
  
  stop("invalid type")
  
}
