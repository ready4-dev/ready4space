#' ready4_sp_local_raw
#' @name ready4_sp_local_raw
#' @description An S4 class to represent Object defining data to be saved in local directory in a raw (unprocessed) format.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot save_type character
#' @slot merge_with_chr_vec character
#' @slot raw_data_dir_chr character
#' @slot pckg_chr character
#' @slot overwrite_lgl logical
#' @slot save_lgl logical
#' @import ready4use
#' @include s4_ready4_lookup.R
methods::setClass(methods::className("ready4_sp_local_raw","ready4use"),
contains = "ready4_local_raw",
slots = c(lup_tbs_r4 = "ready4_lookup"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))

#' ready4_sp_local_raw
#' @name ready4_sp_local_raw
#' @description Create a new S4 object of the class:ready4_sp_local_raw
#' @param save_type character, Default: 'NA'
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param raw_data_dir_chr character, Default: 'NA'
#' @param pckg_chr character, Default: 'NA'
#' @param overwrite_lgl logical, Default: NA
#' @param save_lgl logical, Default: NA
#' @return An S4 object of the ready4_sp_local_raw class
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[methods]{new}}
#' @rdname ready4_sp_local_raw
#' @export
#' @importFrom methods new
ready4_sp_local_raw <- function(save_type = NA_character_,
merge_with_chr_vec = NA_character_,
raw_data_dir_chr = NA_character_,
pckg_chr = NA_character_,
overwrite_lgl = NA,
save_lgl = NA){
methods::new("ready4_sp_local_raw",
save_type = save_type,
merge_with_chr_vec = merge_with_chr_vec,
raw_data_dir_chr = raw_data_dir_chr,
pckg_chr = pckg_chr,
overwrite_lgl = overwrite_lgl,
save_lgl = save_lgl)
}
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local_raw
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x) x@lup_tbs_r4)
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local_raw
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4-set
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@lup_tbs_r4 <- value
methods::validObject(x)
x})
#' save_type<-
#' @name save_type<--ready4_sp_local_raw
#' @description Set the value of the slot save_type for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_type-set
methods::setMethod("save_type<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@save_type <- value
methods::validObject(x)
x})
#' merge_with_chr_vec<-
#' @name merge_with_chr_vec<--ready4_sp_local_raw
#' @description Set the value of the slot merge_with_chr_vec for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname merge_with_chr_vec-set
methods::setMethod("merge_with_chr_vec<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@merge_with_chr_vec <- value
methods::validObject(x)
x})
#' raw_data_dir_chr<-
#' @name raw_data_dir_chr<--ready4_sp_local_raw
#' @description Set the value of the slot raw_data_dir_chr for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname raw_data_dir_chr-set
methods::setMethod("raw_data_dir_chr<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@raw_data_dir_chr <- value
methods::validObject(x)
x})
#' pckg_chr<-
#' @name pckg_chr<--ready4_sp_local_raw
#' @description Set the value of the slot pckg_chr for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pckg_chr-set
methods::setMethod("pckg_chr<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@pckg_chr <- value
methods::validObject(x)
x})
#' overwrite_lgl<-
#' @name overwrite_lgl<--ready4_sp_local_raw
#' @description Set the value of the slot overwrite_lgl for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname overwrite_lgl-set
methods::setMethod("overwrite_lgl<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@overwrite_lgl <- value
methods::validObject(x)
x})
#' save_lgl<-
#' @name save_lgl<--ready4_sp_local_raw
#' @description Set the value of the slot save_lgl for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_lgl-set
methods::setMethod("save_lgl<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@save_lgl <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_sp_local_raw",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
