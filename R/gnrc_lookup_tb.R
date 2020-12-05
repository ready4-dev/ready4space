#' lookup_tb
#' @description S4 Generic function to get the value of the slot lookup_tb
#' @name lookup_tb
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("lookup_tb", function(x) standardGeneric("lookup_tb"))
#' lookup_tb
#' @name lookup_tb-ready4_macro
#' @description Get the value of the slot lookup_tb for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lookup_tb
methods::setMethod("lookup_tb", methods::className("ready4_macro",".GlobalEnv"), function(x) x@lookup_tb)
#' lookup_tb<-
#' @description S4 Generic function to set the value of the slot lookup_tb
#' @name lookup_tb<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("lookup_tb<-", function(x, value) standardGeneric("lookup_tb<-"))
#' lookup_tb<-
#' @name lookup_tb<--ready4_macro
#' @description Set the value of the slot lookup_tb for S4 objects of class ready4_macro
#' @param x An object of class ready4_macro
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lookup_tb-set
methods::setMethod("lookup_tb<-", methods::className("ready4_macro",".GlobalEnv"), function(x, value) {
x@lookup_tb <- value
methods::validObject(x)
x})
