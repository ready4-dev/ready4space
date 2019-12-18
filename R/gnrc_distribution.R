#' distribution
#' @description S4 Generic function to get the value of the slot distribution
#' @name distribution
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("distribution", function(x) standardGeneric("distribution"))
#' distribution<-
#' @description S4 Generic function to set the value of the slot distribution
#' @name distribution<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("distribution<-", function(x, value) standardGeneric("distribution<-"))
