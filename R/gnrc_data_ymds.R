#' data_ymds
#' @description S4 Generic function to get the value of the slot data_ymds
#' @name data_ymds
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("data_ymds", function(x) standardGeneric("data_ymds"))
#' data_ymds
#' @name data_ymds-ready4_profiled_area
#' @description Get the value of the slot data_ymds for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_ymds
methods::setMethod("data_ymds", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@data_ymds)
#' data_ymds<-
#' @description S4 Generic function to set the value of the slot data_ymds
#' @name data_ymds<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("data_ymds<-", function(x, value) standardGeneric("data_ymds<-"))
#' data_ymds<-
#' @name data_ymds<--ready4_profiled_area
#' @description Set the value of the slot data_ymds for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_ymds-set
methods::setMethod("data_ymds<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@data_ymds <- value
methods::validObject(x)
x})
