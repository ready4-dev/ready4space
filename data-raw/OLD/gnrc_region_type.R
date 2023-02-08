#' region_type
#' @description S4 Generic function to get the value of the slot region_type
#' @rdname region_type-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("region_type", function(x) standardGeneric("region_type"))
#' region_type
#' @name region_type-ready4_meso_region
#' @description Get the value of the slot region_type for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @rdname region_type-methods
#' @aliases region_type,ready4_meso_region-method
methods::setMethod("region_type", methods::className("ready4_meso_region"), function (x) 
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
#' @name region_type<--ready4_meso_region
#' @description Set the value of the slot region_type for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @rdname region_type_set-methods
#' @aliases region_type<-,ready4_meso_region-method
methods::setMethod("region_type<-", methods::className("ready4_meso_region"), function (x, value) 
{
    x@region_type <- value
    methods::validObject(x)
    x
})
