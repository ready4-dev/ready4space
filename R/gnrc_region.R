#' region
#' @description S4 Generic function to get the value of the slot region
#' @name region
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("region", function(x) standardGeneric("region"))
#' region
#' @name region-ready4_meso_region
#' @description Get the value of the slot region for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname region
methods::setMethod("region", methods::className("ready4_meso_region",".GlobalEnv"), function(x) x@region)
#' region<-
#' @description S4 Generic function to set the value of the slot region
#' @name region<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("region<-", function(x, value) standardGeneric("region<-"))
#' region<-
#' @name region<--ready4_meso_region
#' @description Set the value of the slot region for S4 objects of class ready4_meso_region
#' @param x An object of class ready4_meso_region
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname region-set
methods::setMethod("region<-", methods::className("ready4_meso_region",".GlobalEnv"), function(x, value) {
x@region <- value
methods::validObject(x)
x})
