#' geom_dist_km_cuts
#' @description S4 Generic function to get the value of the slot geom_dist_km_cuts
#' @name geom_dist_km_cuts
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("geom_dist_km_cuts", function(x) standardGeneric("geom_dist_km_cuts"))
#' geom_dist_km_cuts
#' @name geom_dist_km_cuts-ready4_micro
#' @description Get the value of the slot geom_dist_km_cuts for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geom_dist_km_cuts
methods::setMethod("geom_dist_km_cuts", methods::className("ready4_micro",".GlobalEnv"), function(x) x@geom_dist_km_cuts)
#' geom_dist_km_cuts<-
#' @description S4 Generic function to set the value of the slot geom_dist_km_cuts
#' @name geom_dist_km_cuts<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("geom_dist_km_cuts<-", function(x, value) standardGeneric("geom_dist_km_cuts<-"))
#' geom_dist_km_cuts<-
#' @name geom_dist_km_cuts<--ready4_micro
#' @description Set the value of the slot geom_dist_km_cuts for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geom_dist_km_cuts-set
methods::setMethod("geom_dist_km_cuts<-", methods::className("ready4_micro",".GlobalEnv"), function(x, value) {
x@geom_dist_km_cuts <- value
methods::validObject(x)
x})
