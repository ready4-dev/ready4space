#' geom_dist_limit_km
#' @description S4 Generic function to get the value of the slot geom_dist_limit_km
#' @name geom_dist_limit_km
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("geom_dist_limit_km", function(x) standardGeneric("geom_dist_limit_km"))
#' geom_dist_limit_km
#' @name geom_dist_limit_km-ready4_profiled_area
#' @description Get the value of the slot geom_dist_limit_km for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geom_dist_limit_km
methods::setMethod("geom_dist_limit_km", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@geom_dist_limit_km)
#' geom_dist_limit_km<-
#' @description S4 Generic function to set the value of the slot geom_dist_limit_km
#' @name geom_dist_limit_km<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("geom_dist_limit_km<-", function(x, value) standardGeneric("geom_dist_limit_km<-"))
#' geom_dist_limit_km<-
#' @name geom_dist_limit_km<--ready4_profiled_area
#' @description Set the value of the slot geom_dist_limit_km for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geom_dist_limit_km-set
methods::setMethod("geom_dist_limit_km<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@geom_dist_limit_km <- value
methods::validObject(x)
x})
