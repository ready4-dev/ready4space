#' ready4_profiled_area
#' @name ready4_profiled_area
#' @description An S4 class to represent Information to create a profiled area object
#' @include s4_ready4_lookup.R
#' @slot country character
methods::setClass(methods::className("ready4_profiled_area",".GlobalEnv"),
slots = c(country = "character"),
prototype =  list(country = NA_character_))

#' ready4_profiled_area
#' @name ready4_profiled_area
#' @description Create a new S4 object of the class:ready4_profiled_area
#' @param country character, Default: 'NA'
#' @return An S4 object of the ready4_profiled_area class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_profiled_area
#' @export 
#' @importFrom methods new
ready4_profiled_area <- function(country = NA_character_){ 
methods::new("ready4_profiled_area",
country = country)
}
#' country
#' @name country-ready4_profiled_area
#' @description Get the value of the slot country for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country
methods::setMethod("country", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@country)
#' country<-
#' @name country<--ready4_profiled_area
#' @description Set the value of the slot country for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country-set
methods::setMethod("country<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@country <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_profiled_area",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
