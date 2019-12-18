#' parameter_name
#' @description S4 Generic function to get the value of the slot parameter_name
#' @name parameter_name
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("parameter_name", function(x) standardGeneric("parameter_name"))
#' parameter_name<-
#' @description S4 Generic function to set the value of the slot parameter_name
#' @name parameter_name<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("parameter_name<-", function(x, value) standardGeneric("parameter_name<-"))
