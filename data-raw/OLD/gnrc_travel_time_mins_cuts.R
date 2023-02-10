#' travel_time_mins_cuts
#' @description S4 Generic function to get the value of the slot travel_time_mins_cuts
#' @rdname travel_time_mins_cuts-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("travel_time_mins_cuts", function(x) standardGeneric("travel_time_mins_cuts"))
#' travel_time_mins_cuts
#' @name travel_time_mins_cuts-VicinityMicro
#' @description Get the value of the slot travel_time_mins_cuts for S4 objects of class VicinityMicro
#' @param x An object of class VicinityMicro
#' @rdname travel_time_mins_cuts-methods
#' @aliases travel_time_mins_cuts,VicinityMicro-method
methods::setMethod("travel_time_mins_cuts", methods::className("VicinityMicro"), function (x) 
{
    x@travel_time_mins_cuts
})
#' travel_time_mins_cuts<-
#' @description S4 Generic function to set the value of the slot travel_time_mins_cuts
#' @rdname travel_time_mins_cuts_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("travel_time_mins_cuts<-", function(x, value) standardGeneric("travel_time_mins_cuts<-"))
#' travel_time_mins_cuts<-
#' @name travel_time_mins_cuts<--VicinityMicro
#' @description Set the value of the slot travel_time_mins_cuts for S4 objects of class VicinityMicro
#' @param x An object of class VicinityMicro
#' @rdname travel_time_mins_cuts_set-methods
#' @aliases travel_time_mins_cuts<-,VicinityMicro-method
methods::setMethod("travel_time_mins_cuts<-", methods::className("VicinityMicro"), function (x, value) 
{
    x@travel_time_mins_cuts <- value
    methods::validObject(x)
    x
})
