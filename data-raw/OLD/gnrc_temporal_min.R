#' temporal_min
#' @description S4 Generic function to get the value of the slot temporal_min
#' @rdname temporal_min-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("temporal_min", function(x) standardGeneric("temporal_min"))
#' temporal_min
#' @name temporal_min-VicinityMacro
#' @description Get the value of the slot temporal_min for S4 objects of class VicinityMacro
#' @param x An object of class VicinityMacro
#' @rdname temporal_min-methods
#' @aliases temporal_min,VicinityMacro-method
methods::setMethod("temporal_min", methods::className("VicinityMacro"), function (x) 
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
#' @name temporal_min<--VicinityMacro
#' @description Set the value of the slot temporal_min for S4 objects of class VicinityMacro
#' @param x An object of class VicinityMacro
#' @rdname temporal_min_set-methods
#' @aliases temporal_min<-,VicinityMacro-method
methods::setMethod("temporal_min<-", methods::className("VicinityMacro"), function (x, value) 
{
    x@temporal_min <- value
    methods::validObject(x)
    x
})
