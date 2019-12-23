#' use_coord_lup
#' @description S4 Generic function to get the value of the slot use_coord_lup
#' @name use_coord_lup
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("use_coord_lup", function(x) standardGeneric("use_coord_lup"))
#' use_coord_lup
#' @name use_coord_lup-ready4_profiled_area
#' @description Get the value of the slot use_coord_lup for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname use_coord_lup
methods::setMethod("use_coord_lup", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@use_coord_lup)
#' use_coord_lup<-
#' @description S4 Generic function to set the value of the slot use_coord_lup
#' @name use_coord_lup<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("use_coord_lup<-", function(x, value) standardGeneric("use_coord_lup<-"))
#' use_coord_lup<-
#' @name use_coord_lup<--ready4_profiled_area
#' @description Set the value of the slot use_coord_lup for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname use_coord_lup-set
methods::setMethod("use_coord_lup<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@use_coord_lup <- value
methods::validObject(x)
x})
