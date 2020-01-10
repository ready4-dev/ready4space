#' temporal_min
#' @description S4 Generic function to get the value of the slot temporal_min
#' @name temporal_min
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("temporal_min", function(x) standardGeneric("temporal_min"))
#' temporal_min
#' @name temporal_min-ready4_macro
#' @description Get the value of the slot temporal_min for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname temporal_min
methods::setMethod("temporal_min", methods::className("ready4_macro",".GlobalEnv"), function(x) x@temporal_min)
#' temporal_min<-
#' @description S4 Generic function to set the value of the slot temporal_min
#' @name temporal_min<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("temporal_min<-", function(x, value) standardGeneric("temporal_min<-"))
#' temporal_min<-
#' @name temporal_min<--ready4_macro
#' @description Set the value of the slot temporal_min for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname temporal_min-set
methods::setMethod("temporal_min<-", methods::className("ready4_macro",".GlobalEnv"), function(x, value) {
x@temporal_min <- value
methods::validObject(x)
x})
