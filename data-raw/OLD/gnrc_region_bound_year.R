#' region_bound_year
#' @description S4 Generic function to get the value of the slot region_bound_year
#' @rdname region_bound_year-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("region_bound_year", function(x) standardGeneric("region_bound_year"))
#' region_bound_year
#' @name region_bound_year-VicinityMesoRegion
#' @description Get the value of the slot region_bound_year for S4 objects of class VicinityMesoRegion
#' @param x An object of class VicinityMesoRegion
#' @rdname region_bound_year-methods
#' @aliases region_bound_year,VicinityMesoRegion-method
methods::setMethod("region_bound_year", methods::className("VicinityMesoRegion"), function (x) 
{
    x@region_bound_year
})
#' region_bound_year<-
#' @description S4 Generic function to set the value of the slot region_bound_year
#' @rdname region_bound_year_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("region_bound_year<-", function(x, value) standardGeneric("region_bound_year<-"))
#' region_bound_year<-
#' @name region_bound_year<--VicinityMesoRegion
#' @description Set the value of the slot region_bound_year for S4 objects of class VicinityMesoRegion
#' @param x An object of class VicinityMesoRegion
#' @rdname region_bound_year_set-methods
#' @aliases region_bound_year<-,VicinityMesoRegion-method
methods::setMethod("region_bound_year<-", methods::className("VicinityMesoRegion"), function (x, value) 
{
    x@region_bound_year <- value
    methods::validObject(x)
    x
})
