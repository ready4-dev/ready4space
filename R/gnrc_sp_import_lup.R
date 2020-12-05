#' sp_import_lup
#' @description S4 Generic function to get the value of the slot sp_import_lup
#' @name sp_import_lup
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("sp_import_lup", function(x) standardGeneric("sp_import_lup"))
#' sp_import_lup
#' @name sp_import_lup-ready4_lookup
#' @description Get the value of the slot sp_import_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_import_lup
methods::setMethod("sp_import_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_import_lup)
#' sp_import_lup<-
#' @description S4 Generic function to set the value of the slot sp_import_lup
#' @name sp_import_lup<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("sp_import_lup<-", function(x, value) standardGeneric("sp_import_lup<-"))
#' sp_import_lup<-
#' @name sp_import_lup<--ready4_lookup
#' @description Set the value of the slot sp_import_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_import_lup-set
methods::setMethod("sp_import_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_import_lup <- value
methods::validObject(x)
x})
