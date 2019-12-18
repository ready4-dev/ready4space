#' st_data
#' @description S4 Generic function to get the value of the slot st_data
#' @name st_data
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("st_data", function(x) standardGeneric("st_data"))
#' st_data<-
#' @description S4 Generic function to set the value of the slot st_data
#' @name st_data<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("st_data<-", function(x, value) standardGeneric("st_data<-"))
