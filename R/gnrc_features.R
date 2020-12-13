#' features
#' @description S4 Generic function to get the value of the slot features
#' @rdname features-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("features", function(x) standardGeneric("features"))
#' features
#' @name features-ready4_profiled_area
#' @description Get the value of the slot features for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname features-methods
#' @aliases features,ready4_profiled_area-method
methods::setMethod("features", methods::className("ready4_profiled_area"), function (x) 
{
    x@features
})
#' features<-
#' @description S4 Generic function to set the value of the slot features
#' @rdname features_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("features<-", function(x, value) standardGeneric("features<-"))
#' features<-
#' @name features<--ready4_profiled_area
#' @description Set the value of the slot features for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname features_set-methods
#' @aliases features<-,ready4_profiled_area-method
methods::setMethod("features<-", methods::className("ready4_profiled_area"), function (x, value) 
{
    x@features <- value
    methods::validObject(x)
    x
})
