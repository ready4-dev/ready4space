#' region_type
#' @description S4 Generic function to get the value of the slot region_type
#' @rdname region_type-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("region_type", function(x) standardGeneric("region_type"))
#' region_type
#' @name region_type-VicinityMesoRegion
#' @description Get the value of the slot region_type for S4 objects of class VicinityMesoRegion
#' @param x An object of class VicinityMesoRegion
#' @rdname region_type-methods
#' @aliases region_type,VicinityMesoRegion-method
methods::setMethod("region_type", methods::className("VicinityMesoRegion"), function (x) 
{
    x@region_type
})
#' region_type<-
#' @description S4 Generic function to set the value of the slot region_type
#' @rdname region_type_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("region_type<-", function(x, value) standardGeneric("region_type<-"))
#' region_type<-
#' @name region_type<--VicinityMesoRegion
#' @description Set the value of the slot region_type for S4 objects of class VicinityMesoRegion
#' @param x An object of class VicinityMesoRegion
#' @rdname region_type_set-methods
#' @aliases region_type<-,VicinityMesoRegion-method
methods::setMethod("region_type<-", methods::className("VicinityMesoRegion"), function (x, value) 
{
    x@region_type <- value
    methods::validObject(x)
    x
})
