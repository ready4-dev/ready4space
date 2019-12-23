#' ready4_lookup
#' @name ready4_lookup
#' @description An S4 class to represent Look up tables to use throughout readyforwhatsnext suite
setOldClass(c("ready4_sp_abbreviations_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_import_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_data_pack_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_resolution_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_site_coord_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_starter_sf_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_uid_lup","tbl_df", "tbl", "data.frame"))
#' @slot sp_abbreviations_lup ready4_sp_abbreviations_lup
#' @slot sp_import_lup ready4_sp_import_lup
#' @slot sp_data_pack_lup ready4_sp_data_pack_lup
#' @slot sp_resolution_lup ready4_sp_resolution_lup
#' @slot sp_site_coord_lup ready4_sp_site_coord_lup
#' @slot sp_starter_sf_lup ready4_sp_starter_sf_lup
#' @slot sp_uid_lup ready4_sp_uid_lup
methods::setClass(methods::className("ready4_lookup",".GlobalEnv"),
slots = c(sp_abbreviations_lup = "ready4_sp_abbreviations_lup",sp_import_lup = "ready4_sp_import_lup",sp_data_pack_lup = "ready4_sp_data_pack_lup",sp_resolution_lup = "ready4_sp_resolution_lup",sp_site_coord_lup = "ready4_sp_site_coord_lup",sp_starter_sf_lup = "ready4_sp_starter_sf_lup",sp_uid_lup = "ready4_sp_uid_lup"),
prototype =  list(sp_abbreviations_lup = ready4_sp_abbreviations_lup(),sp_import_lup = ready4_sp_import_lup(),sp_data_pack_lup = ready4_sp_data_pack_lup(),sp_resolution_lup = ready4_sp_resolution_lup(),sp_site_coord_lup = ready4_sp_site_coord_lup(),sp_starter_sf_lup = ready4_sp_starter_sf_lup(),sp_uid_lup = ready4_sp_uid_lup()))

#' ready4_lookup
#' @name ready4_lookup
#' @description Create a new S4 object of the class:ready4_lookup
#' @param sp_abbreviations_lup ready4_sp_abbreviations_lup, Default: ready4_sp_abbreviations_lup()
#' @param sp_import_lup ready4_sp_import_lup, Default: ready4_sp_import_lup()
#' @param sp_data_pack_lup ready4_sp_data_pack_lup, Default: ready4_sp_data_pack_lup()
#' @param sp_resolution_lup ready4_sp_resolution_lup, Default: ready4_sp_resolution_lup()
#' @param sp_site_coord_lup ready4_sp_site_coord_lup, Default: ready4_sp_site_coord_lup()
#' @param sp_starter_sf_lup ready4_sp_starter_sf_lup, Default: ready4_sp_starter_sf_lup()
#' @param sp_uid_lup ready4_sp_uid_lup, Default: ready4_sp_uid_lup()
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
ready4_lookup <- function(sp_abbreviations_lup = ready4_sp_abbreviations_lup(),
sp_import_lup = ready4_sp_import_lup(),
sp_data_pack_lup = ready4_sp_data_pack_lup(),
sp_resolution_lup = ready4_sp_resolution_lup(),
sp_site_coord_lup = ready4_sp_site_coord_lup(),
sp_starter_sf_lup = ready4_sp_starter_sf_lup(),
sp_uid_lup = ready4_sp_uid_lup()){ 
methods::new("ready4_lookup",
sp_abbreviations_lup = sp_abbreviations_lup,
sp_import_lup = sp_import_lup,
sp_data_pack_lup = sp_data_pack_lup,
sp_resolution_lup = sp_resolution_lup,
sp_site_coord_lup = sp_site_coord_lup,
sp_starter_sf_lup = sp_starter_sf_lup,
sp_uid_lup = sp_uid_lup)
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
#' sp_data_pack_lup
#' @name sp_data_pack_lup-ready4_lookup
#' @description Get the value of the slot sp_data_pack_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_data_pack_lup
methods::setMethod("sp_data_pack_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_data_pack_lup)
#' sp_data_pack_lup<-
#' @name sp_data_pack_lup<--ready4_lookup
#' @description Set the value of the slot sp_data_pack_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_data_pack_lup-set
methods::setMethod("sp_data_pack_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_data_pack_lup <- value
methods::validObject(x)
x})
#' sp_resolution_lup
#' @name sp_resolution_lup-ready4_lookup
#' @description Get the value of the slot sp_resolution_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_resolution_lup
methods::setMethod("sp_resolution_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_resolution_lup)
#' sp_resolution_lup<-
#' @name sp_resolution_lup<--ready4_lookup
#' @description Set the value of the slot sp_resolution_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_resolution_lup-set
methods::setMethod("sp_resolution_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_resolution_lup <- value
methods::validObject(x)
x})
#' sp_site_coord_lup
#' @name sp_site_coord_lup-ready4_lookup
#' @description Get the value of the slot sp_site_coord_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_site_coord_lup
methods::setMethod("sp_site_coord_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_site_coord_lup)
#' sp_site_coord_lup<-
#' @name sp_site_coord_lup<--ready4_lookup
#' @description Set the value of the slot sp_site_coord_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_site_coord_lup-set
methods::setMethod("sp_site_coord_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_site_coord_lup <- value
methods::validObject(x)
x})
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
#' sp_uid_lup
#' @name sp_uid_lup-ready4_lookup
#' @description Get the value of the slot sp_uid_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_uid_lup
methods::setMethod("sp_uid_lup", methods::className("ready4_lookup",".GlobalEnv"), function(x) x@sp_uid_lup)
#' sp_uid_lup<-
#' @name sp_uid_lup<--ready4_lookup
#' @description Set the value of the slot sp_uid_lup for S4 objects of class ready4_lookup
#' @param x An object of class ready4_lookup
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sp_uid_lup-set
methods::setMethod("sp_uid_lup<-", methods::className("ready4_lookup",".GlobalEnv"), function(x, value) {
x@sp_uid_lup <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_lookup",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
