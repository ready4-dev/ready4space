#' area_bound_year
#' @description S4 Generic function to get the value of the slot area_bound_year
#' @rdname area_bound_year-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("area_bound_year", function(x) standardGeneric("area_bound_year"))
#' area_bound_year
#' @name area_bound_year-ready4_meso_area
#' @description Get the value of the slot area_bound_year for S4 objects of class ready4_meso_area
#' @param x An object of class ready4_meso_area
#' @rdname area_bound_year-methods
#' @aliases area_bound_year,ready4_meso_area-method
methods::setMethod("area_bound_year", methods::className("ready4_meso_area"), function (x) 
{
    x@area_bound_year
})
#' area_bound_year<-
#' @description S4 Generic function to set the value of the slot area_bound_year
#' @rdname area_bound_year_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("area_bound_year<-", function(x, value) standardGeneric("area_bound_year<-"))
#' area_bound_year<-
#' @name area_bound_year<--ready4_meso_area
#' @description Set the value of the slot area_bound_year for S4 objects of class ready4_meso_area
#' @param x An object of class ready4_meso_area
#' @rdname area_bound_year_set-methods
#' @aliases area_bound_year<-,ready4_meso_area-method
methods::setMethod("area_bound_year<-", methods::className("ready4_meso_area"), function (x, value) 
{
    x@area_bound_year <- value
    methods::validObject(x)
    x
})
