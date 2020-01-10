#' travel_time_mins_cuts
#' @description S4 Generic function to get the value of the slot travel_time_mins_cuts
#' @name travel_time_mins_cuts
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("travel_time_mins_cuts", function(x) standardGeneric("travel_time_mins_cuts"))
#' travel_time_mins_cuts
#' @name travel_time_mins_cuts-ready4_micro
#' @description Get the value of the slot travel_time_mins_cuts for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname travel_time_mins_cuts
methods::setMethod("travel_time_mins_cuts", methods::className("ready4_micro",".GlobalEnv"), function(x) x@travel_time_mins_cuts)
#' travel_time_mins_cuts<-
#' @description S4 Generic function to set the value of the slot travel_time_mins_cuts
#' @name travel_time_mins_cuts<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("travel_time_mins_cuts<-", function(x, value) standardGeneric("travel_time_mins_cuts<-"))
#' travel_time_mins_cuts<-
#' @name travel_time_mins_cuts<--ready4_micro
#' @description Set the value of the slot travel_time_mins_cuts for S4 objects of class ready4_micro
#' @param x An object of class ready4_micro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname travel_time_mins_cuts-set
methods::setMethod("travel_time_mins_cuts<-", methods::className("ready4_micro",".GlobalEnv"), function(x, value) {
x@travel_time_mins_cuts <- value
methods::validObject(x)
x})
