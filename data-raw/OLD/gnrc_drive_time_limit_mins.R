#' drive_time_limit_mins
#' @description S4 Generic function to get the value of the slot drive_time_limit_mins
#' @rdname drive_time_limit_mins-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("drive_time_limit_mins", function(x) standardGeneric("drive_time_limit_mins"))
#' drive_time_limit_mins
#' @name drive_time_limit_mins-ready4_profiled_area
#' @description Get the value of the slot drive_time_limit_mins for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname drive_time_limit_mins-methods
#' @aliases drive_time_limit_mins,ready4_profiled_area-method
methods::setMethod("drive_time_limit_mins", methods::className("ready4_profiled_area"), function (x) 
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
#' @name drive_time_limit_mins<--ready4_profiled_area
#' @description Set the value of the slot drive_time_limit_mins for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname drive_time_limit_mins_set-methods
#' @aliases drive_time_limit_mins<-,ready4_profiled_area-method
methods::setMethod("drive_time_limit_mins<-", methods::className("ready4_profiled_area"), function (x, value) 
{
    x@drive_time_limit_mins <- value
    methods::validObject(x)
    x
})
