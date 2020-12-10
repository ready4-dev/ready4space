#' sp_abbreviations_lup
#' @name sp_abbreviations_lup-ready4_lookup
#' @description Get the value of the slot sp_abbreviations_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_abbreviations_lup-methods
#' @aliases sp_abbreviations_lup,ready4_lookup-method
methods::setMethod("sp_abbreviations_lup", methods::className("ready4_lookup"), function (x) 
{
    x@sp_abbreviations_lup
})
#' sp_abbreviations_lup<-
#' @name sp_abbreviations_lup<--ready4_lookup
#' @description Set the value of the slot sp_abbreviations_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @rdname sp_abbreviations_lup_set-methods
#' @aliases sp_abbreviations_lup<-,ready4_lookup-method
methods::setMethod("sp_abbreviations_lup<-", methods::className("ready4_lookup"), function (x, value) 
{
    x@sp_abbreviations_lup <- value
    methods::validObject(x)
    x
})
