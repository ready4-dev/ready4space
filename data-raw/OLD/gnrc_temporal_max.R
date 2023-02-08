#' temporal_max
#' @description S4 Generic function to get the value of the slot temporal_max
#' @rdname temporal_max-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("temporal_max", function(x) standardGeneric("temporal_max"))
#' temporal_max
#' @name temporal_max-ready4_macro
#' @description Get the value of the slot temporal_max for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname temporal_max-methods
#' @aliases temporal_max,ready4_macro-method
methods::setMethod("temporal_max", methods::className("ready4_macro"), function (x) 
{
    x@temporal_max
})
#' temporal_max<-
#' @description S4 Generic function to set the value of the slot temporal_max
#' @rdname temporal_max_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("temporal_max<-", function(x, value) standardGeneric("temporal_max<-"))
#' temporal_max<-
#' @name temporal_max<--ready4_macro
#' @description Set the value of the slot temporal_max for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname temporal_max_set-methods
#' @aliases temporal_max<-,ready4_macro-method
methods::setMethod("temporal_max<-", methods::className("ready4_macro"), function (x, value) 
{
    x@temporal_max <- value
    methods::validObject(x)
    x
})
