#' par_vals
#' @description S4 Generic function to get the value of the slot par_vals
#' @rdname par_vals-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("par_vals", function(x) standardGeneric("par_vals"))
#' par_vals
#' @name par_vals-ready4_env
#' @description Get the value of the slot par_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @rdname par_vals-methods
#' @aliases par_vals,ready4_env-method
methods::setMethod("par_vals", methods::className("ready4_env"), function (x) 
{
    x@par_vals
})
#' par_vals<-
#' @description S4 Generic function to set the value of the slot par_vals
#' @rdname par_vals_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("par_vals<-", function(x, value) standardGeneric("par_vals<-"))
#' par_vals<-
#' @name par_vals<--ready4_env
#' @description Set the value of the slot par_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @rdname par_vals_set-methods
#' @aliases par_vals<-,ready4_env-method
methods::setMethod("par_vals<-", methods::className("ready4_env"), function (x, value) 
{
    x@par_vals <- value
    methods::validObject(x)
    x
})
