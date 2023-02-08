#' sp_starter_sf_lup
#' @description S4 Generic function to get the value of the slot sp_starter_sf_lup
#' @rdname sp_starter_sf_lup-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("sp_starter_sf_lup", function(x) standardGeneric("sp_starter_sf_lup"))
#' sp_starter_sf_lup
#' @name sp_starter_sf_lup-ready4_lookup
#' @description Get the value of the slot sp_starter_sf_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_starter_sf_lup-methods
#' @aliases sp_starter_sf_lup,ready4_lookup-method
methods::setMethod("sp_starter_sf_lup", methods::className("ready4_lookup"), function (x) 
{
    x@sp_starter_sf_lup
})
#' sp_starter_sf_lup<-
#' @description S4 Generic function to set the value of the slot sp_starter_sf_lup
#' @rdname sp_starter_sf_lup_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("sp_starter_sf_lup<-", function(x, value) standardGeneric("sp_starter_sf_lup<-"))
#' sp_starter_sf_lup<-
#' @name sp_starter_sf_lup<--ready4_lookup
#' @description Set the value of the slot sp_starter_sf_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_starter_sf_lup_set-methods
#' @aliases sp_starter_sf_lup<-,ready4_lookup-method
methods::setMethod("sp_starter_sf_lup<-", methods::className("ready4_lookup"), function (x, value) 
{
    x@sp_starter_sf_lup <- value
    methods::validObject(x)
    x
})
