#' temporal_max
#' @description S4 Generic function to get the value of the slot temporal_max
#' @name temporal_max
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("temporal_max", function(x) standardGeneric("temporal_max"))
#' temporal_max
#' @name temporal_max-ready4_macro
#' @description Get the value of the slot temporal_max for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname temporal_max
methods::setMethod("temporal_max", methods::className("ready4_macro",".GlobalEnv"), function(x) x@temporal_max)
#' temporal_max<-
#' @description S4 Generic function to set the value of the slot temporal_max
#' @name temporal_max<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("temporal_max<-", function(x, value) standardGeneric("temporal_max<-"))
#' temporal_max<-
#' @name temporal_max<--ready4_macro
#' @description Set the value of the slot temporal_max for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname temporal_max-set
methods::setMethod("temporal_max<-", methods::className("ready4_macro",".GlobalEnv"), function(x, value) {
x@temporal_max <- value
methods::validObject(x)
x})
