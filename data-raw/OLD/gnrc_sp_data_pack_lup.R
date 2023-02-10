#' sp_data_pack_lup
#' @description S4 Generic function to get the value of the slot sp_data_pack_lup
#' @rdname sp_data_pack_lup-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("sp_data_pack_lup", function(x) standardGeneric("sp_data_pack_lup"))
#' sp_data_pack_lup
#' @name sp_data_pack_lup-VicinityLookup
#' @description Get the value of the slot sp_data_pack_lup for S4 objects of class VicinityLookup
#' @param x An object of class VicinityLookup
#' @rdname sp_data_pack_lup-methods
#' @aliases sp_data_pack_lup,VicinityLookup-method
methods::setMethod("sp_data_pack_lup", methods::className("VicinityLookup"), function (x) 
{
    x@sp_data_pack_lup
})
#' sp_data_pack_lup<-
#' @description S4 Generic function to set the value of the slot sp_data_pack_lup
#' @rdname sp_data_pack_lup_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("sp_data_pack_lup<-", function(x, value) standardGeneric("sp_data_pack_lup<-"))
#' sp_data_pack_lup<-
#' @name sp_data_pack_lup<--VicinityLookup
#' @description Set the value of the slot sp_data_pack_lup for S4 objects of class VicinityLookup
#' @param x An object of class VicinityLookup
#' @rdname sp_data_pack_lup_set-methods
#' @aliases sp_data_pack_lup<-,VicinityLookup-method
methods::setMethod("sp_data_pack_lup<-", methods::className("VicinityLookup"), function (x, value) 
{
    x@sp_data_pack_lup <- value
    methods::validObject(x)
    x
})
