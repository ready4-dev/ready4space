#' region
#' @description S4 Generic function to get the value of the slot region
#' @rdname region-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("region", function(x) standardGeneric("region"))
#' region
#' @name region-ready4_meso_region
#' @description Get the value of the slot region for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @rdname region-methods
#' @aliases region,ready4_meso_region-method
methods::setMethod("region", methods::className("ready4_meso_region"), function (x) 
{
    x@region
})
#' region<-
#' @description S4 Generic function to set the value of the slot region
#' @rdname region_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("region<-", function(x, value) standardGeneric("region<-"))
#' region<-
#' @name region<--ready4_meso_region
#' @description Set the value of the slot region for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @rdname region_set-methods
#' @aliases region<-,ready4_meso_region-method
methods::setMethod("region<-", methods::className("ready4_meso_region"), function (x, value) 
{
    x@region <- value
    methods::validObject(x)
    x
})
