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
