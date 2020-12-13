#' data_ymds
#' @description S4 Generic function to get the value of the slot data_ymds
#' @rdname data_ymds-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("data_ymds", function(x) standardGeneric("data_ymds"))
#' data_ymds
#' @name data_ymds-ready4_profiled_area
#' @description Get the value of the slot data_ymds for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname data_ymds-methods
#' @aliases data_ymds,ready4_profiled_area-method
methods::setMethod("data_ymds", methods::className("ready4_profiled_area"), function (x) 
{
    x@data_ymds
})
#' data_ymds<-
#' @description S4 Generic function to set the value of the slot data_ymds
#' @rdname data_ymds_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("data_ymds<-", function(x, value) standardGeneric("data_ymds<-"))
#' data_ymds<-
#' @name data_ymds<--ready4_profiled_area
#' @description Set the value of the slot data_ymds for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname data_ymds_set-methods
#' @aliases data_ymds<-,ready4_profiled_area-method
methods::setMethod("data_ymds<-", methods::className("ready4_profiled_area"), function (x, value) 
{
    x@data_ymds <- value
    methods::validObject(x)
    x
})
