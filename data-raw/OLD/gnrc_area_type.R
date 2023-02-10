#' area_type
#' @description S4 Generic function to get the value of the slot area_type
#' @rdname area_type-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("area_type", function(x) standardGeneric("area_type"))
#' area_type
#' @name area_type-VicinityMesoArea
#' @description Get the value of the slot area_type for S4 objects of class VicinityMesoArea
#' @param x An object of class VicinityMesoArea
#' @rdname area_type-methods
#' @aliases area_type,VicinityMesoArea-method
methods::setMethod("area_type", methods::className("VicinityMesoArea"), function (x) 
{
    x@area_type
})
#' area_type<-
#' @description S4 Generic function to set the value of the slot area_type
#' @rdname area_type_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("area_type<-", function(x, value) standardGeneric("area_type<-"))
#' area_type<-
#' @name area_type<--VicinityMesoArea
#' @description Set the value of the slot area_type for S4 objects of class VicinityMesoArea
#' @param x An object of class VicinityMesoArea
#' @rdname area_type_set-methods
#' @aliases area_type<-,VicinityMesoArea-method
methods::setMethod("area_type<-", methods::className("VicinityMesoArea"), function (x, value) 
{
    x@area_type <- value
    methods::validObject(x)
    x
})
