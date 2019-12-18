#' st_envir
#' @description S4 Generic function to get the value of the slot st_envir
#' @name st_envir
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("st_envir", function(x) standardGeneric("st_envir"))
#' st_envir<-
#' @description S4 Generic function to set the value of the slot st_envir
#' @name st_envir<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("st_envir<-", function(x, value) standardGeneric("st_envir<-"))
