#' country
#' @description S4 Generic function to get the value of the slot country
#' @rdname country-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("country", function(x) standardGeneric("country"))
#' country
#' @name country-ready4_macro
#' @description Get the value of the slot country for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname country-methods
#' @aliases country,ready4_macro-method
methods::setMethod("country", methods::className("ready4_macro"), function (x) 
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
#' @name country<--ready4_macro
#' @description Set the value of the slot country for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @rdname country_set-methods
#' @aliases country<-,ready4_macro-method
methods::setMethod("country<-", methods::className("ready4_macro"), function (x, value) 
{
    x@country <- value
    methods::validObject(x)
    x
})
