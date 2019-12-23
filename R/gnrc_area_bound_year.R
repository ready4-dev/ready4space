#' area_bound_year
#' @description S4 Generic function to get the value of the slot area_bound_year
#' @name area_bound_year
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("area_bound_year", function(x) standardGeneric("area_bound_year"))
#' area_bound_year
#' @name area_bound_year-ready4_profiled_area
#' @description Get the value of the slot area_bound_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_bound_year
methods::setMethod("area_bound_year", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@area_bound_year)
#' area_bound_year<-
#' @description S4 Generic function to set the value of the slot area_bound_year
#' @name area_bound_year<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("area_bound_year<-", function(x, value) standardGeneric("area_bound_year<-"))
#' area_bound_year<-
#' @name area_bound_year<--ready4_profiled_area
#' @description Set the value of the slot area_bound_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_bound_year-set
methods::setMethod("area_bound_year<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@area_bound_year <- value
methods::validObject(x)
x})
