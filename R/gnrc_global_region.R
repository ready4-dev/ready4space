#' global_region
#' @description S4 Generic function to get the value of the slot global_region
#' @name global_region
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("global_region", function(x) standardGeneric("global_region"))
#' global_region
#' @name global_region-ready4_macro
#' @description Get the value of the slot global_region for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname global_region
methods::setMethod("global_region", methods::className("ready4_macro",".GlobalEnv"), function(x) x@global_region)
#' global_region<-
#' @description S4 Generic function to set the value of the slot global_region
#' @name global_region<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("global_region<-", function(x, value) standardGeneric("global_region<-"))
#' global_region<-
#' @name global_region<--ready4_macro
#' @description Set the value of the slot global_region for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname global_region-set
methods::setMethod("global_region<-", methods::className("ready4_macro",".GlobalEnv"), function(x, value) {
x@global_region <- value
methods::validObject(x)
x})
