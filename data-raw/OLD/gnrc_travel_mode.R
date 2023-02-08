#' travel_mode
#' @description S4 Generic function to get the value of the slot travel_mode
#' @rdname travel_mode-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("travel_mode", function(x) standardGeneric("travel_mode"))
#' travel_mode
#' @name travel_mode-ready4_micro
#' @description Get the value of the slot travel_mode for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @rdname travel_mode-methods
#' @aliases travel_mode,ready4_micro-method
methods::setMethod("travel_mode", methods::className("ready4_micro"), function (x) 
{
    x@travel_mode
})
#' travel_mode<-
#' @description S4 Generic function to set the value of the slot travel_mode
#' @rdname travel_mode_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("travel_mode<-", function(x, value) standardGeneric("travel_mode<-"))
#' travel_mode<-
#' @name travel_mode<--ready4_micro
#' @description Set the value of the slot travel_mode for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @rdname travel_mode_set-methods
#' @aliases travel_mode<-,ready4_micro-method
methods::setMethod("travel_mode<-", methods::className("ready4_micro"), function (x, value) 
{
    x@travel_mode <- value
    methods::validObject(x)
    x
})
