#' travel_time_mins_cuts
#' @description S4 Generic function to get the value of the slot travel_time_mins_cuts
#' @rdname travel_time_mins_cuts-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("travel_time_mins_cuts", function(x) standardGeneric("travel_time_mins_cuts"))
#' travel_time_mins_cuts
#' @name travel_time_mins_cuts-ready4_micro
#' @description Get the value of the slot travel_time_mins_cuts for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @rdname travel_time_mins_cuts-methods
#' @aliases travel_time_mins_cuts,ready4_micro-method
methods::setMethod("travel_time_mins_cuts", methods::className("ready4_micro"), function (x) 
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
#' @name travel_time_mins_cuts<--ready4_micro
#' @description Set the value of the slot travel_time_mins_cuts for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @rdname travel_time_mins_cuts_set-methods
#' @aliases travel_time_mins_cuts<-,ready4_micro-method
methods::setMethod("travel_time_mins_cuts<-", methods::className("ready4_micro"), function (x, value) 
{
    x@travel_time_mins_cuts <- value
    methods::validObject(x)
    x
})
