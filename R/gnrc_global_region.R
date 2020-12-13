#' global_region
#' @description S4 Generic function to get the value of the slot global_region
#' @rdname global_region-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("global_region", function(x) standardGeneric("global_region"))
#' global_region
#' @name global_region-ready4_macro
#' @description Get the value of the slot global_region for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname global_region-methods
#' @aliases global_region,ready4_macro-method
methods::setMethod("global_region", methods::className("ready4_macro"), function (x) 
{
    x@global_region
})
#' global_region<-
#' @description S4 Generic function to set the value of the slot global_region
#' @rdname global_region_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("global_region<-", function(x, value) standardGeneric("global_region<-"))
#' global_region<-
#' @name global_region<--ready4_macro
#' @description Set the value of the slot global_region for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname global_region_set-methods
#' @aliases global_region<-,ready4_macro-method
methods::setMethod("global_region<-", methods::className("ready4_macro"), function (x, value) 
{
    x@global_region <- value
    methods::validObject(x)
    x
})
