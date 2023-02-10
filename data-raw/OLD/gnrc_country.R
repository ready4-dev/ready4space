#' country
#' @description S4 Generic function to get the value of the slot country
#' @rdname country-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("country", function(x) standardGeneric("country"))
#' country
#' @name country-VicinityMacro
#' @description Get the value of the slot country for S4 objects of class VicinityMacro
#' @param x An object of class VicinityMacro
#' @rdname country-methods
#' @aliases country,VicinityMacro-method
methods::setMethod("country", methods::className("VicinityMacro"), function (x) 
{
    x@country
})
#' country<-
#' @description S4 Generic function to set the value of the slot country
#' @rdname country_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("country<-", function(x, value) standardGeneric("country<-"))
#' country<-
#' @name country<--VicinityMacro
#' @description Set the value of the slot country for S4 objects of class VicinityMacro
#' @param x An object of class VicinityMacro
#' @rdname country_set-methods
#' @aliases country<-,VicinityMacro-method
methods::setMethod("country<-", methods::className("VicinityMacro"), function (x, value) 
{
    x@country <- value
    methods::validObject(x)
    x
})
