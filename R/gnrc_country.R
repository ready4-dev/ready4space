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
#' country
#' @name country-ready4_profiled_area
#' @description Get the value of the slot country for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country
methods::setMethod("country", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@country)
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
#' country<-
#' @name country<--ready4_profiled_area
#' @description Set the value of the slot country for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country-set
methods::setMethod("country<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@country <- value
methods::validObject(x)
x})
