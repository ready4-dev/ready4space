#' st_data
#' @description S4 Generic function to get the value of the slot st_data
#' @rdname st_data-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("st_data", function(x) standardGeneric("st_data"))
#' st_data
#' @name st_data-VicinitySpaceTime
#' @description Get the value of the slot st_data for S4 objects of class VicinitySpaceTime
#' @param x An object of class VicinitySpaceTime
#' @rdname st_data-methods
#' @aliases st_data,VicinitySpaceTime-method
methods::setMethod("st_data", methods::className("VicinitySpaceTime"), function (x) 
{
    x@st_data
})
#' st_data<-
#' @description S4 Generic function to set the value of the slot st_data
#' @rdname st_data_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("st_data<-", function(x, value) standardGeneric("st_data<-"))
#' st_data<-
#' @name st_data<--VicinitySpaceTime
#' @description Set the value of the slot st_data for S4 objects of class VicinitySpaceTime
#' @param x An object of class VicinitySpaceTime
#' @rdname st_data_set-methods
#' @aliases st_data<-,VicinitySpaceTime-method
methods::setMethod("st_data<-", methods::className("VicinitySpaceTime"), function (x, value) 
{
    x@st_data <- value
    methods::validObject(x)
    x
})
