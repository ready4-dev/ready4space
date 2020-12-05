#' region_bound_year
#' @description S4 Generic function to get the value of the slot region_bound_year
#' @name region_bound_year
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("region_bound_year", function(x) standardGeneric("region_bound_year"))
#' region_bound_year
#' @name region_bound_year-ready4_meso_region
#' @description Get the value of the slot region_bound_year for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname region_bound_year
methods::setMethod("region_bound_year", methods::className("ready4_meso_region",".GlobalEnv"), function(x) x@region_bound_year)
#' region_bound_year<-
#' @description S4 Generic function to set the value of the slot region_bound_year
#' @name region_bound_year<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("region_bound_year<-", function(x, value) standardGeneric("region_bound_year<-"))
#' region_bound_year<-
#' @name region_bound_year<--ready4_meso_region
#' @description Set the value of the slot region_bound_year for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname region_bound_year-set
methods::setMethod("region_bound_year<-", methods::className("ready4_meso_region",".GlobalEnv"), function(x, value) {
x@region_bound_year <- value
methods::validObject(x)
x})
