#' nbr_bands
#' @description S4 Generic function to get the value of the slot nbr_bands
#' @name nbr_bands
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("nbr_bands", function(x) standardGeneric("nbr_bands"))
#' nbr_bands
#' @name nbr_bands-ready4_profiled_area
#' @description Get the value of the slot nbr_bands for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nbr_bands
methods::setMethod("nbr_bands", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@nbr_bands)
#' nbr_bands<-
#' @description S4 Generic function to set the value of the slot nbr_bands
#' @name nbr_bands<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("nbr_bands<-", function(x, value) standardGeneric("nbr_bands<-"))
#' nbr_bands<-
#' @name nbr_bands<--ready4_profiled_area
#' @description Set the value of the slot nbr_bands for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nbr_bands-set
methods::setMethod("nbr_bands<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@nbr_bands <- value
methods::validObject(x)
x})
