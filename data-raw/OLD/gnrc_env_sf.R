#' env_sf
#' @description S4 Generic function to get the value of the slot env_sf
#' @rdname env_sf-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("env_sf", function(x) standardGeneric("env_sf"))
#' env_sf
#' @name env_sf-VicinitySpaceTime
#' @description Get the value of the slot env_sf for S4 objects of class VicinitySpaceTime
#' @param x An object of class VicinitySpaceTime
#' @rdname env_sf-methods
#' @aliases env_sf,VicinitySpaceTime-method
methods::setMethod("env_sf", methods::className("VicinitySpaceTime"), function (x) 
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
#' @name env_sf<--VicinitySpaceTime
#' @description Set the value of the slot env_sf for S4 objects of class VicinitySpaceTime
#' @param x An object of class VicinitySpaceTime
#' @rdname env_sf_set-methods
#' @aliases env_sf<-,VicinitySpaceTime-method
methods::setMethod("env_sf<-", methods::className("VicinitySpaceTime"), function (x, value) 
{
    x@env_sf <- value
    methods::validObject(x)
    x
})
