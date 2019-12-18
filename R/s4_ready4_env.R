#' ready4_env
#' @name ready4_env
#' @description An S4 class to represent Spatiotemporal environment
#' @slot st_data list
methods::setClass(methods::className("ready4_env",".GlobalEnv"),
slots = c(st_data = "list"),
prototype =  list(st_data = list(list())))

#' ready4_env
#' @name ready4_env
#' @description Create a new S4 object of the class:ready4_env
#' @param st_data list, Default: list(list())
#' @return An S4 object of the ready4_env class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_env
#' @export 
#' @importFrom methods new
ready4_env <- function(st_data = list(list())){ 
methods::new("ready4_env",
st_data = st_data)
}
#' st_data
#' @name st_data-ready4_env
#' @description Get the value of the slot st_data for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname st_data
methods::setMethod("st_data", methods::className("ready4_env",".GlobalEnv"), function(x) x@st_data)
#' st_data<-
#' @name st_data<--ready4_env
#' @description Set the value of the slot st_data for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname st_data-set
methods::setMethod("st_data<-", methods::className("ready4_env",".GlobalEnv"), function(x, value) {
x@st_data <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_env",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
