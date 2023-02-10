#' lookup_tb
#' @description S4 Generic function to get the value of the slot lookup_tb
#' @rdname lookup_tb-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("lookup_tb", function(x) standardGeneric("lookup_tb"))
#' lookup_tb
#' @name lookup_tb-VicinityMacro
#' @description Get the value of the slot lookup_tb for S4 objects of class VicinityMacro
#' @param x An object of class VicinityMacro
#' @rdname lookup_tb-methods
#' @aliases lookup_tb,VicinityMacro-method
methods::setMethod("lookup_tb", methods::className("VicinityMacro"), function (x) 
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
#' @name lookup_tb<--VicinityMacro
#' @description Set the value of the slot lookup_tb for S4 objects of class VicinityMacro
#' @param x An object of class VicinityMacro
#' @rdname lookup_tb_set-methods
#' @aliases lookup_tb<-,VicinityMacro-method
methods::setMethod("lookup_tb<-", methods::className("VicinityMacro"), function (x, value) 
{
    x@lookup_tb <- value
    methods::validObject(x)
    x
})
