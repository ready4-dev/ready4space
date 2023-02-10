#' region
#' @description S4 Generic function to get the value of the slot region
#' @rdname region-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("region", function(x) standardGeneric("region"))
#' region
#' @name region-VicinityMesoRegion
#' @description Get the value of the slot region for S4 objects of class VicinityMesoRegion
#' @param x An object of class VicinityMesoRegion
#' @rdname region-methods
#' @aliases region,VicinityMesoRegion-method
methods::setMethod("region", methods::className("VicinityMesoRegion"), function (x) 
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
#' @name region<--VicinityMesoRegion
#' @description Set the value of the slot region for S4 objects of class VicinityMesoRegion
#' @param x An object of class VicinityMesoRegion
#' @rdname region_set-methods
#' @aliases region<-,VicinityMesoRegion-method
methods::setMethod("region<-", methods::className("VicinityMesoRegion"), function (x, value) 
{
    x@region <- value
    methods::validObject(x)
    x
})
