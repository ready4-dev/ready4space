#' ready4_struc
#' @name ready4_struc
#' @description An S4 class to represent Structural parameter probability distributions and transformations
#' @slot parameter_name character
methods::setClass(methods::className("ready4_struc",".GlobalEnv"),
slots = c(parameter_name = "character"),
prototype =  list(parameter_name = NA_character_))

#' ready4_struc
#' @name ready4_struc
#' @description Create a new S4 object of the class:ready4_struc
#' @param parameter_name character, Default: 'NA'
#' @return An S4 object of the ready4_struc class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_struc
#' @export 
#' @importFrom methods new
ready4_struc <- function(parameter_name = NA_character_){ 
methods::new("ready4_struc",
parameter_name = parameter_name)
}
#' parameter_name
#' @name parameter_name-ready4_struc
#' @description Get the value of the slot parameter_name for S4 objects of class ready4_struc
#' @param x An object of class ready4_struc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname parameter_name
methods::setMethod("parameter_name", methods::className("ready4_struc",".GlobalEnv"), function(x) x@parameter_name)
#' parameter_name<-
#' @name parameter_name<--ready4_struc
#' @description Set the value of the slot parameter_name for S4 objects of class ready4_struc
#' @param x An object of class ready4_struc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname parameter_name-set
methods::setMethod("parameter_name<-", methods::className("ready4_struc",".GlobalEnv"), function(x, value) {
x@parameter_name <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_struc",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
