#' sp_resolution_lup
#' @description S4 Generic function to get the value of the slot sp_resolution_lup
#' @rdname sp_resolution_lup-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("sp_resolution_lup", function(x) standardGeneric("sp_resolution_lup"))
#' sp_resolution_lup
#' @name sp_resolution_lup-ready4_lookup
#' @description Get the value of the slot sp_resolution_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_resolution_lup-methods
#' @aliases sp_resolution_lup,ready4_lookup-method
methods::setMethod("sp_resolution_lup", methods::className("ready4_lookup"), function (x) 
{
    x@sp_resolution_lup
})
#' sp_resolution_lup<-
#' @description S4 Generic function to set the value of the slot sp_resolution_lup
#' @rdname sp_resolution_lup_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("sp_resolution_lup<-", function(x, value) standardGeneric("sp_resolution_lup<-"))
#' sp_resolution_lup<-
#' @name sp_resolution_lup<--ready4_lookup
#' @description Set the value of the slot sp_resolution_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_resolution_lup_set-methods
#' @aliases sp_resolution_lup<-,ready4_lookup-method
methods::setMethod("sp_resolution_lup<-", methods::className("ready4_lookup"), function (x, value) 
{
    x@sp_resolution_lup <- value
    methods::validObject(x)
    x
})
