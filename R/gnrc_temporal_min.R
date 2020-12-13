#' temporal_min
#' @description S4 Generic function to get the value of the slot temporal_min
#' @rdname temporal_min-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("temporal_min", function(x) standardGeneric("temporal_min"))
#' temporal_min
#' @name temporal_min-ready4_macro
#' @description Get the value of the slot temporal_min for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname temporal_min-methods
#' @aliases temporal_min,ready4_macro-method
methods::setMethod("temporal_min", methods::className("ready4_macro"), function (x) 
{
    x@temporal_min
})
#' temporal_min<-
#' @description S4 Generic function to set the value of the slot temporal_min
#' @rdname temporal_min_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("temporal_min<-", function(x, value) standardGeneric("temporal_min<-"))
#' temporal_min<-
#' @name temporal_min<--ready4_macro
#' @description Set the value of the slot temporal_min for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname temporal_min_set-methods
#' @aliases temporal_min<-,ready4_macro-method
methods::setMethod("temporal_min<-", methods::className("ready4_macro"), function (x, value) 
{
    x@temporal_min <- value
    methods::validObject(x)
    x
})
