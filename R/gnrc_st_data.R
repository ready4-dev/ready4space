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
#' st_data
#' @name st_data-ready4_env
#' @description Get the value of the slot st_data for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname st_data
methods::setMethod("st_data", methods::className("ready4_env",".GlobalEnv"), function(x) x@st_data)
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
#' st_data<-
#' @name st_data<--ready4_env
#' @description Set the value of the slot st_data for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname st_data-set
methods::setMethod("st_data<-", methods::className("ready4_env",".GlobalEnv"), function(x, value) {
x@st_data <- value
methods::validObject(x)
x})
