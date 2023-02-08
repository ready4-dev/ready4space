#' nbr_bands
#' @description S4 Generic function to get the value of the slot nbr_bands
#' @rdname nbr_bands-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("nbr_bands", function(x) standardGeneric("nbr_bands"))
#' nbr_bands
#' @name nbr_bands-ready4_profiled_area
#' @description Get the value of the slot nbr_bands for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname nbr_bands-methods
#' @aliases nbr_bands,ready4_profiled_area-method
methods::setMethod("nbr_bands", methods::className("ready4_profiled_area"), function (x) 
{
    x@nbr_bands
})
#' nbr_bands<-
#' @description S4 Generic function to set the value of the slot nbr_bands
#' @rdname nbr_bands_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("nbr_bands<-", function(x, value) standardGeneric("nbr_bands<-"))
#' nbr_bands<-
#' @name nbr_bands<--ready4_profiled_area
#' @description Set the value of the slot nbr_bands for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname nbr_bands_set-methods
#' @aliases nbr_bands<-,ready4_profiled_area-method
methods::setMethod("nbr_bands<-", methods::className("ready4_profiled_area"), function (x, value) 
{
    x@nbr_bands <- value
    methods::validObject(x)
    x
})
