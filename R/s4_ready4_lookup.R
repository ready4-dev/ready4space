#' ready4_lookup
#' @name ready4_lookup
#' @description An S4 class to represent Look up tables to use throughout readyforwhatsnext suite
setOldClass(c("ready4_sp_abbreviations_lup","tbl_df", "tbl", "data.frame"))#' @slot sp_abbreviations_lup ready4_sp_abbreviations_lup
methods::setClass(methods::className("ready4_lookup",".GlobalEnv"),
slots = c(sp_abbreviations_lup = "ready4_sp_abbreviations_lup"),
prototype =  list(sp_abbreviations_lup = ready4_sp_abbreviations_lup()))

#' ready4_lookup
#' @name ready4_lookup
#' @description Create a new S4 object of the class:ready4_lookup
#' @param sp_abbreviations_lup ready4_sp_abbreviations_lup, Default: ready4_sp_abbreviations_lup()
#' @return An S4 object of the ready4_lookup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_lookup
#' @export 
#' @importFrom methods new
ready4_lookup <- function(sp_abbreviations_lup = ready4_sp_abbreviations_lup()){ 
methods::new("ready4_lookup",
sp_abbreviations_lup = sp_abbreviations_lup)
}
#' sp_abbreviations_lup
#' @name sp_abbreviations_lup-ready4_lookup
#' @description Get the value of the slot sp_abbreviations_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_abbreviations_lup
methods::setMethod("sp_abbreviations_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_abbreviations_lup)
#' sp_abbreviations_lup<-
#' @name sp_abbreviations_lup<--ready4_lookup
#' @description Set the value of the slot sp_abbreviations_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_abbreviations_lup-set
methods::setMethod("sp_abbreviations_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_abbreviations_lup <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_lookup",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
