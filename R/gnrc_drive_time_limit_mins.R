#' drive_time_limit_mins
#' @description S4 Generic function to get the value of the slot drive_time_limit_mins
#' @name drive_time_limit_mins
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("drive_time_limit_mins", function(x) standardGeneric("drive_time_limit_mins"))
#' drive_time_limit_mins
#' @name drive_time_limit_mins-ready4_profiled_area
#' @description Get the value of the slot drive_time_limit_mins for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname drive_time_limit_mins
methods::setMethod("drive_time_limit_mins", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@drive_time_limit_mins)
#' drive_time_limit_mins<-
#' @description S4 Generic function to set the value of the slot drive_time_limit_mins
#' @name drive_time_limit_mins<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("drive_time_limit_mins<-", function(x, value) standardGeneric("drive_time_limit_mins<-"))
#' drive_time_limit_mins<-
#' @name drive_time_limit_mins<--ready4_profiled_area
#' @description Set the value of the slot drive_time_limit_mins for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname drive_time_limit_mins-set
methods::setMethod("drive_time_limit_mins<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@drive_time_limit_mins <- value
methods::validObject(x)
x})
