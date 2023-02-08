#' lup_tbs_r4
#' @description S4 Generic function to get the value of the slot lup_tbs_r4
#' @rdname lup_tbs_r4-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("lup_tbs_r4", function(x) standardGeneric("lup_tbs_r4"))
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,ready4_sp_local-method
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @description S4 Generic function to set the value of the slot lup_tbs_r4
#' @rdname lup_tbs_r4_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("lup_tbs_r4<-", function(x, value) standardGeneric("lup_tbs_r4<-"))
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,ready4_sp_local-method
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_spRaw
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_spRaw
#' @param x An object of class ready4_spRaw
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,ready4_spRaw-method
methods::setMethod("lup_tbs_r4", methods::className("ready4_spRaw"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_spRaw
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_spRaw
#' @param x An object of class ready4_spRaw
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,ready4_spRaw-method
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_spRaw"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_spProcessed
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_spProcessed
#' @param x An object of class ready4_spProcessed
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,ready4_spProcessed-method
methods::setMethod("lup_tbs_r4", methods::className("ready4_spProcessed"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_spProcessed
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_spProcessed
#' @param x An object of class ready4_spProcessed
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,ready4_spProcessed-method
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_spProcessed"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
