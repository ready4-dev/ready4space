#' drive_time_limit_mins
#' @description S4 Generic function to get the value of the slot drive_time_limit_mins
#' @rdname drive_time_limit_mins-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("drive_time_limit_mins", function(x) standardGeneric("drive_time_limit_mins"))
#' drive_time_limit_mins
#' @name drive_time_limit_mins-VicinityProfile
#' @description Get the value of the slot drive_time_limit_mins for S4 objects of class VicinityProfile
#' @param x An object of class VicinityProfile
#' @rdname drive_time_limit_mins-methods
#' @aliases drive_time_limit_mins,VicinityProfile-method
methods::setMethod("drive_time_limit_mins", methods::className("VicinityProfile"), function (x) 
{
    x@drive_time_limit_mins
})
#' drive_time_limit_mins<-
#' @description S4 Generic function to set the value of the slot drive_time_limit_mins
#' @rdname drive_time_limit_mins_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("drive_time_limit_mins<-", function(x, value) standardGeneric("drive_time_limit_mins<-"))
#' drive_time_limit_mins<-
#' @name drive_time_limit_mins<--VicinityProfile
#' @description Set the value of the slot drive_time_limit_mins for S4 objects of class VicinityProfile
#' @param x An object of class VicinityProfile
#' @rdname drive_time_limit_mins_set-methods
#' @aliases drive_time_limit_mins<-,VicinityProfile-method
methods::setMethod("drive_time_limit_mins<-", methods::className("VicinityProfile"), function (x, value) 
{
    x@drive_time_limit_mins <- value
    methods::validObject(x)
    x
})
