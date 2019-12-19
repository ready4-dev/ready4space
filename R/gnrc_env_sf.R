#' env_sf
#' @description S4 Generic function to get the value of the slot env_sf
#' @name env_sf
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("env_sf", function(x) standardGeneric("env_sf"))
#' env_sf<-
#' @description S4 Generic function to set the value of the slot env_sf
#' @name env_sf<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("env_sf<-", function(x, value) standardGeneric("env_sf<-"))
