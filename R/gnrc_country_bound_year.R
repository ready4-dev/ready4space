#' country_bound_year
#' @description S4 Generic function to get the value of the slot country_bound_year
#' @name country_bound_year
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("country_bound_year", function(x) standardGeneric("country_bound_year"))
#' country_bound_year
#' @name country_bound_year-ready4_macro
#' @description Get the value of the slot country_bound_year for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country_bound_year
methods::setMethod("country_bound_year", methods::className("ready4_macro",".GlobalEnv"), function(x) x@country_bound_year)
#' country_bound_year<-
#' @description S4 Generic function to set the value of the slot country_bound_year
#' @name country_bound_year<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("country_bound_year<-", function(x, value) standardGeneric("country_bound_year<-"))
#' country_bound_year<-
#' @name country_bound_year<--ready4_macro
#' @description Set the value of the slot country_bound_year for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country_bound_year-set
methods::setMethod("country_bound_year<-", methods::className("ready4_macro",".GlobalEnv"), function(x, value) {
x@country_bound_year <- value
methods::validObject(x)
x})
