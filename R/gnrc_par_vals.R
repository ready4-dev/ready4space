#' par_vals
#' @description S4 Generic function to get the value of the slot par_vals
#' @name par_vals
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("par_vals", function(x) standardGeneric("par_vals"))
#' par_vals
#' @name par_vals-ready4_env
#' @description Get the value of the slot par_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname par_vals
methods::setMethod("par_vals", methods::className("ready4_env",".GlobalEnv"), function(x) x@par_vals)
#' par_vals<-
#' @description S4 Generic function to set the value of the slot par_vals
#' @name par_vals<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("par_vals<-", function(x, value) standardGeneric("par_vals<-"))
#' par_vals<-
#' @name par_vals<--ready4_env
#' @description Set the value of the slot par_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname par_vals-set
methods::setMethod("par_vals<-", methods::className("ready4_env",".GlobalEnv"), function(x, value) {
x@par_vals <- value
methods::validObject(x)
x})
