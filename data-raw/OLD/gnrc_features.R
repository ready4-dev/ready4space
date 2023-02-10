#' features
#' @description S4 Generic function to get the value of the slot features
#' @rdname features-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("features", function(x) standardGeneric("features"))
#' features
#' @name features-VicinityProfile
#' @description Get the value of the slot features for S4 objects of class VicinityProfile
#' @param x An object of class VicinityProfile
#' @rdname features-methods
#' @aliases features,VicinityProfile-method
methods::setMethod("features", methods::className("VicinityProfile"), function (x) 
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
#' @name features<--VicinityProfile
#' @description Set the value of the slot features for S4 objects of class VicinityProfile
#' @param x An object of class VicinityProfile
#' @rdname features_set-methods
#' @aliases features<-,VicinityProfile-method
methods::setMethod("features<-", methods::className("VicinityProfile"), function (x, value) 
{
    x@features <- value
    methods::validObject(x)
    x
})
