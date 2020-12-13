#' area
#' @description S4 Generic function to get the value of the slot area
#' @rdname area-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("area", function(x) standardGeneric("area"))
#' area
#' @name area-ready4_meso_area
#' @description Get the value of the slot area for S4 objects of class ready4_meso_area
#' @param x An object of class ready4_meso_area
#' @rdname area-methods
#' @aliases area,ready4_meso_area-method
methods::setMethod("area", methods::className("ready4_meso_area"), function (x) 
{
    x@area
})
#' area<-
#' @description S4 Generic function to set the value of the slot area
#' @rdname area_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("area<-", function(x, value) standardGeneric("area<-"))
#' area<-
#' @name area<--ready4_meso_area
#' @description Set the value of the slot area for S4 objects of class ready4_meso_area
#' @param x An object of class ready4_meso_area
#' @rdname area_set-methods
#' @aliases area<-,ready4_meso_area-method
methods::setMethod("area<-", methods::className("ready4_meso_area"), function (x, value) 
{
    x@area <- value
    methods::validObject(x)
    x
})
