#' country
#' @description S4 Generic function to get the value of the slot country
#' @name country
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("country", function(x) standardGeneric("country"))
#' country<-
#' @description S4 Generic function to set the value of the slot country
#' @name country<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("country<-", function(x, value) standardGeneric("country<-"))
