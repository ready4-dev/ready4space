#' data_year
#' @description S4 Generic function to get the value of the slot data_year
#' @name data_year
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("data_year", function(x) standardGeneric("data_year"))
#' data_year
#' @name data_year-ready4_profiled_area
#' @description Get the value of the slot data_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_year
methods::setMethod("data_year", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@data_year)
#' data_year<-
#' @description S4 Generic function to set the value of the slot data_year
#' @name data_year<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("data_year<-", function(x, value) standardGeneric("data_year<-"))
#' data_year<-
#' @name data_year<--ready4_profiled_area
#' @description Set the value of the slot data_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_year-set
methods::setMethod("data_year<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@data_year <- value
methods::validObject(x)
x})
