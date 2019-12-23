#' crs_nbr
#' @description S4 Generic function to get the value of the slot crs_nbr
#' @name crs_nbr
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("crs_nbr", function(x) standardGeneric("crs_nbr"))
#' crs_nbr
#' @name crs_nbr-ready4_profiled_area
#' @description Get the value of the slot crs_nbr for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname crs_nbr
methods::setMethod("crs_nbr", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@crs_nbr)
#' crs_nbr<-
#' @description S4 Generic function to set the value of the slot crs_nbr
#' @name crs_nbr<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("crs_nbr<-", function(x, value) standardGeneric("crs_nbr<-"))
#' crs_nbr<-
#' @name crs_nbr<--ready4_profiled_area
#' @description Set the value of the slot crs_nbr for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname crs_nbr-set
methods::setMethod("crs_nbr<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@crs_nbr <- value
methods::validObject(x)
x})
