#' features
#' @description S4 Generic function to get the value of the slot features
#' @name features
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("features", function(x) standardGeneric("features"))
#' features<-
#' @description S4 Generic function to set the value of the slot features
#' @name features<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("features<-", function(x, value) standardGeneric("features<-"))
