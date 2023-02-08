#' sp_site_coord_lup
#' @description S4 Generic function to get the value of the slot sp_site_coord_lup
#' @rdname sp_site_coord_lup-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("sp_site_coord_lup", function(x) standardGeneric("sp_site_coord_lup"))
#' sp_site_coord_lup
#' @name sp_site_coord_lup-ready4_lookup
#' @description Get the value of the slot sp_site_coord_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_site_coord_lup-methods
#' @aliases sp_site_coord_lup,ready4_lookup-method
methods::setMethod("sp_site_coord_lup", methods::className("ready4_lookup"), function (x) 
{
    x@sp_site_coord_lup
})
#' sp_site_coord_lup<-
#' @description S4 Generic function to set the value of the slot sp_site_coord_lup
#' @rdname sp_site_coord_lup_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("sp_site_coord_lup<-", function(x, value) standardGeneric("sp_site_coord_lup<-"))
#' sp_site_coord_lup<-
#' @name sp_site_coord_lup<--ready4_lookup
#' @description Set the value of the slot sp_site_coord_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_site_coord_lup_set-methods
#' @aliases sp_site_coord_lup<-,ready4_lookup-method
methods::setMethod("sp_site_coord_lup<-", methods::className("ready4_lookup"), function (x, value) 
{
    x@sp_site_coord_lup <- value
    methods::validObject(x)
    x
})
