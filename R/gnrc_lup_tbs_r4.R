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
#' @name lup_tbs_r4-ready4_sp_local_raw
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,ready4_sp_local_raw-method
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local_raw"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local_raw
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,ready4_sp_local_raw-method
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local_raw"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local_proc
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,ready4_sp_local_proc-method
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local_proc"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local_proc
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,ready4_sp_local_proc-method
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local_proc"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
