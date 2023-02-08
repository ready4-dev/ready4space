#' param_vals
#' @description S4 Generic function to get the value of the slot param_vals
#' @rdname param_vals-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("param_vals", function(x) standardGeneric("param_vals"))
#' param_vals
#' @name param_vals-ready4_env
#' @description Get the value of the slot param_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @rdname param_vals-methods
#' @aliases param_vals,ready4_env-method
methods::setMethod("param_vals", methods::className("ready4_env"), function (x) 
{
    x@param_vals
})
#' param_vals<-
#' @description S4 Generic function to set the value of the slot param_vals
#' @rdname param_vals_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("param_vals<-", function(x, value) standardGeneric("param_vals<-"))
#' param_vals<-
#' @name param_vals<--ready4_env
#' @description Set the value of the slot param_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @rdname param_vals_set-methods
#' @aliases param_vals<-,ready4_env-method
methods::setMethod("param_vals<-", methods::className("ready4_env"), function (x, value) 
{
    x@param_vals <- value
    methods::validObject(x)
    x
})
