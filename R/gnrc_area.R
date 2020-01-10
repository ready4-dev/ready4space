#' area
#' @description S4 Generic function to get the value of the slot area
#' @name area
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("area", function(x) standardGeneric("area"))
#' area
#' @name area-ready4_meso_area
#' @description Get the value of the slot area for S4 objects of class ready4_meso_area
#' @param x An object of class ready4_meso_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area
methods::setMethod("area", methods::className("ready4_meso_area",".GlobalEnv"), function(x) x@area)
#' area<-
#' @description S4 Generic function to set the value of the slot area
#' @name area<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("area<-", function(x, value) standardGeneric("area<-"))
#' area<-
#' @name area<--ready4_meso_area
#' @description Set the value of the slot area for S4 objects of class ready4_meso_area
#' @param x An object of class ready4_meso_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area-set
methods::setMethod("area<-", methods::className("ready4_meso_area",".GlobalEnv"), function(x, value) {
x@area <- value
methods::validObject(x)
x})
