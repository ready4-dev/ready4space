#' features
#' @description S4 Generic function to get the value of the slot features
#' @name features
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("features", function(x) standardGeneric("features"))
#' features
#' @name features-ready4_profiled_area
#' @description Get the value of the slot features for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname features
methods::setMethod("features", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@features)
#' features<-
#' @description S4 Generic function to set the value of the slot features
#' @name features<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("features<-", function(x, value) standardGeneric("features<-"))
#' features<-
#' @name features<--ready4_profiled_area
#' @description Set the value of the slot features for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname features-set
methods::setMethod("features<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@features <- value
methods::validObject(x)
x})
