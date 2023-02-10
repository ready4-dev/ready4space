#' sp_uid_lup
#' @description S4 Generic function to get the value of the slot sp_uid_lup
#' @rdname sp_uid_lup-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("sp_uid_lup", function(x) standardGeneric("sp_uid_lup"))
#' sp_uid_lup
#' @name sp_uid_lup-VicinityLookup
#' @description Get the value of the slot sp_uid_lup for S4 objects of class VicinityLookup
#' @param x An object of class VicinityLookup
#' @rdname sp_uid_lup-methods
#' @aliases sp_uid_lup,VicinityLookup-method
methods::setMethod("sp_uid_lup", methods::className("VicinityLookup"), function (x) 
{
    x@sp_uid_lup
})
#' sp_uid_lup<-
#' @description S4 Generic function to set the value of the slot sp_uid_lup
#' @rdname sp_uid_lup_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("sp_uid_lup<-", function(x, value) standardGeneric("sp_uid_lup<-"))
#' sp_uid_lup<-
#' @name sp_uid_lup<--VicinityLookup
#' @description Set the value of the slot sp_uid_lup for S4 objects of class VicinityLookup
#' @param x An object of class VicinityLookup
#' @rdname sp_uid_lup_set-methods
#' @aliases sp_uid_lup<-,VicinityLookup-method
methods::setMethod("sp_uid_lup<-", methods::className("VicinityLookup"), function (x, value) 
{
    x@sp_uid_lup <- value
    methods::validObject(x)
    x
})
