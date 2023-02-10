#' geom_dist_km_cuts
#' @description S4 Generic function to get the value of the slot geom_dist_km_cuts
#' @rdname geom_dist_km_cuts-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("geom_dist_km_cuts", function(x) standardGeneric("geom_dist_km_cuts"))
#' geom_dist_km_cuts
#' @name geom_dist_km_cuts-VicinityMicro
#' @description Get the value of the slot geom_dist_km_cuts for S4 objects of class VicinityMicro
#' @param x An object of class VicinityMicro
#' @rdname geom_dist_km_cuts-methods
#' @aliases geom_dist_km_cuts,VicinityMicro-method
methods::setMethod("geom_dist_km_cuts", methods::className("VicinityMicro"), function (x) 
{
    x@geom_dist_km_cuts
})
#' geom_dist_km_cuts<-
#' @description S4 Generic function to set the value of the slot geom_dist_km_cuts
#' @rdname geom_dist_km_cuts_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("geom_dist_km_cuts<-", function(x, value) standardGeneric("geom_dist_km_cuts<-"))
#' geom_dist_km_cuts<-
#' @name geom_dist_km_cuts<--VicinityMicro
#' @description Set the value of the slot geom_dist_km_cuts for S4 objects of class VicinityMicro
#' @param x An object of class VicinityMicro
#' @rdname geom_dist_km_cuts_set-methods
#' @aliases geom_dist_km_cuts<-,VicinityMicro-method
methods::setMethod("geom_dist_km_cuts<-", methods::className("VicinityMicro"), function (x, value) 
{
    x@geom_dist_km_cuts <- value
    methods::validObject(x)
    x
})
