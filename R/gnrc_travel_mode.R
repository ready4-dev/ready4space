#' travel_mode
#' @description S4 Generic function to get the value of the slot travel_mode
#' @name travel_mode
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("travel_mode", function(x) standardGeneric("travel_mode"))
#' travel_mode
#' @name travel_mode-ready4_micro
#' @description Get the value of the slot travel_mode for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname travel_mode
methods::setMethod("travel_mode", methods::className("ready4_micro",".GlobalEnv"), function(x) x@travel_mode)
#' travel_mode<-
#' @description S4 Generic function to set the value of the slot travel_mode
#' @name travel_mode<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("travel_mode<-", function(x, value) standardGeneric("travel_mode<-"))
#' travel_mode<-
#' @name travel_mode<--ready4_micro
#' @description Set the value of the slot travel_mode for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname travel_mode-set
methods::setMethod("travel_mode<-", methods::className("ready4_micro",".GlobalEnv"), function(x, value) {
x@travel_mode <- value
methods::validObject(x)
x})
