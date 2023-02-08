#' lookup_tb
#' @description S4 Generic function to get the value of the slot lookup_tb
#' @rdname lookup_tb-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("lookup_tb", function(x) standardGeneric("lookup_tb"))
#' lookup_tb
#' @name lookup_tb-ready4_macro
#' @description Get the value of the slot lookup_tb for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname lookup_tb-methods
#' @aliases lookup_tb,ready4_macro-method
methods::setMethod("lookup_tb", methods::className("ready4_macro"), function (x) 
{
    x@lookup_tb
})
#' lookup_tb<-
#' @description S4 Generic function to set the value of the slot lookup_tb
#' @rdname lookup_tb_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("lookup_tb<-", function(x, value) standardGeneric("lookup_tb<-"))
#' lookup_tb<-
#' @name lookup_tb<--ready4_macro
#' @description Set the value of the slot lookup_tb for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname lookup_tb_set-methods
#' @aliases lookup_tb<-,ready4_macro-method
methods::setMethod("lookup_tb<-", methods::className("ready4_macro"), function (x, value) 
{
    x@lookup_tb <- value
    methods::validObject(x)
    x
})
