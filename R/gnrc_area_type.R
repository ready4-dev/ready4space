#' area_type
#' @description S4 Generic function to get the value of the slot area_type
#' @name area_type
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("area_type", function(x) standardGeneric("area_type"))
#' area_type
#' @name area_type-ready4_profiled_area
#' @description Get the value of the slot area_type for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_type
methods::setMethod("area_type", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@area_type)
#' area_type<-
#' @description S4 Generic function to set the value of the slot area_type
#' @name area_type<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("area_type<-", function(x, value) standardGeneric("area_type<-"))
#' area_type<-
#' @name area_type<--ready4_profiled_area
#' @description Set the value of the slot area_type for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_type-set
methods::setMethod("area_type<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@area_type <- value
methods::validObject(x)
x})
