#' lup_tbs_r4
#' @description S4 Generic function to get the value of the slot lup_tbs_r4
#' @rdname lup_tbs_r4-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("lup_tbs_r4", function(x) standardGeneric("lup_tbs_r4"))
#' lup_tbs_r4
#' @name lup_tbs_r4-VicinityLocal
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class VicinityLocal
#' @param x An object of class VicinityLocal
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,VicinityLocal-method
methods::setMethod("lup_tbs_r4", methods::className("VicinityLocal"), function (x) 
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
#' @name lup_tbs_r4<--VicinityLocal
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class VicinityLocal
#' @param x An object of class VicinityLocal
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,VicinityLocal-method
methods::setMethod("lup_tbs_r4<-", methods::className("VicinityLocal"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
#' lup_tbs_r4
#' @name lup_tbs_r4-VicinityLocalRaw
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class VicinityLocalRaw
#' @param x An object of class VicinityLocalRaw
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,VicinityLocalRaw-method
methods::setMethod("lup_tbs_r4", methods::className("VicinityLocalRaw"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--VicinityLocalRaw
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class VicinityLocalRaw
#' @param x An object of class VicinityLocalRaw
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,VicinityLocalRaw-method
methods::setMethod("lup_tbs_r4<-", methods::className("VicinityLocalRaw"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
#' lup_tbs_r4
#' @name lup_tbs_r4-VicinityLocalProcessed
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class VicinityLocalProcessed
#' @param x An object of class VicinityLocalProcessed
#' @rdname lup_tbs_r4-methods
#' @aliases lup_tbs_r4,VicinityLocalProcessed-method
methods::setMethod("lup_tbs_r4", methods::className("VicinityLocalProcessed"), function (x) 
{
    x@lup_tbs_r4
})
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--VicinityLocalProcessed
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class VicinityLocalProcessed
#' @param x An object of class VicinityLocalProcessed
#' @rdname lup_tbs_r4_set-methods
#' @aliases lup_tbs_r4<-,VicinityLocalProcessed-method
methods::setMethod("lup_tbs_r4<-", methods::className("VicinityLocalProcessed"), function (x, value) 
{
    x@lup_tbs_r4 <- value
    methods::validObject(x)
    x
})
