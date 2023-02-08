#' env_sf
#' @description S4 Generic function to get the value of the slot env_sf
#' @rdname env_sf-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("env_sf", function(x) standardGeneric("env_sf"))
#' env_sf
#' @name env_sf-ready4_env
#' @description Get the value of the slot env_sf for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @rdname env_sf-methods
#' @aliases env_sf,ready4_env-method
methods::setMethod("env_sf", methods::className("ready4_env"), function (x) 
{
    x@env_sf
})
#' env_sf<-
#' @description S4 Generic function to set the value of the slot env_sf
#' @rdname env_sf_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("env_sf<-", function(x, value) standardGeneric("env_sf<-"))
#' env_sf<-
#' @name env_sf<--ready4_env
#' @description Set the value of the slot env_sf for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @rdname env_sf_set-methods
#' @aliases env_sf<-,ready4_env-method
methods::setMethod("env_sf<-", methods::className("ready4_env"), function (x, value) 
{
    x@env_sf <- value
    methods::validObject(x)
    x
})
