#' geom_dist_limit_km
#' @description S4 Generic function to get the value of the slot geom_dist_limit_km
#' @rdname geom_dist_limit_km-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("geom_dist_limit_km", function(x) standardGeneric("geom_dist_limit_km"))
#' geom_dist_limit_km
#' @name geom_dist_limit_km-VicinityProfile
#' @description Get the value of the slot geom_dist_limit_km for S4 objects of class VicinityProfile
#' @param x An object of class VicinityProfile
#' @rdname geom_dist_limit_km-methods
#' @aliases geom_dist_limit_km,VicinityProfile-method
methods::setMethod("geom_dist_limit_km", methods::className("VicinityProfile"), function (x) 
{
    x@geom_dist_limit_km
})
#' geom_dist_limit_km<-
#' @description S4 Generic function to set the value of the slot geom_dist_limit_km
#' @rdname geom_dist_limit_km_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("geom_dist_limit_km<-", function(x, value) standardGeneric("geom_dist_limit_km<-"))
#' geom_dist_limit_km<-
#' @name geom_dist_limit_km<--VicinityProfile
#' @description Set the value of the slot geom_dist_limit_km for S4 objects of class VicinityProfile
#' @param x An object of class VicinityProfile
#' @rdname geom_dist_limit_km_set-methods
#' @aliases geom_dist_limit_km<-,VicinityProfile-method
methods::setMethod("geom_dist_limit_km<-", methods::className("VicinityProfile"), function (x, value) 
{
    x@geom_dist_limit_km <- value
    methods::validObject(x)
    x
})
