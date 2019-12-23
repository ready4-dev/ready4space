#' sp_starter_sf_lup
#' @description S4 Generic function to get the value of the slot sp_starter_sf_lup
#' @name sp_starter_sf_lup
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("sp_starter_sf_lup", function(x) standardGeneric("sp_starter_sf_lup"))
#' sp_starter_sf_lup
#' @name sp_starter_sf_lup-ready4_lookup
#' @description Get the value of the slot sp_starter_sf_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_starter_sf_lup
methods::setMethod("sp_starter_sf_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_starter_sf_lup)
#' sp_starter_sf_lup<-
#' @description S4 Generic function to set the value of the slot sp_starter_sf_lup
#' @name sp_starter_sf_lup<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("sp_starter_sf_lup<-", function(x, value) standardGeneric("sp_starter_sf_lup<-"))
#' sp_starter_sf_lup<-
#' @name sp_starter_sf_lup<--ready4_lookup
#' @description Set the value of the slot sp_starter_sf_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_starter_sf_lup-set
methods::setMethod("sp_starter_sf_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_starter_sf_lup <- value
methods::validObject(x)
x})
