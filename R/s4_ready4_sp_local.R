#' ready4_sp_local
#' @name ready4_sp_local
#' @description An S4 class to represent Object defining data to be saved in local directory.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot merge_with_chr_vec character
#' @slot raw_data_dir_chr character
#' @slot pckg_chr character
#' @slot overwrite_lgl logical
#' @slot save_lgl logical
#' @import ready4use
methods::setClass(methods::className("ready4_sp_local","ready4use"),
contains = "ready4_local",
slots = c(lup_tbs_r4 = "ready4_lookup"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))

#' ready4_sp_local
#' @name ready4_sp_local
#' @description Create a new S4 object of the class:ready4_sp_local
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param raw_data_dir_chr character, Default: 'NA'
#' @param pckg_chr character, Default: 'NA'
#' @param overwrite_lgl logical, Default: NA
#' @param save_lgl logical, Default: NA
#' @return An S4 object of the ready4_sp_local class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_sp_local
#' @export 
#' @importFrom methods new
ready4_sp_local <- function(merge_with_chr_vec = NA_character_,
raw_data_dir_chr = NA_character_,
pckg_chr = NA_character_,
overwrite_lgl = NA,
save_lgl = NA){ 
methods::new("ready4_sp_local",
merge_with_chr_vec = merge_with_chr_vec,
raw_data_dir_chr = raw_data_dir_chr,
pckg_chr = pckg_chr,
overwrite_lgl = overwrite_lgl,
save_lgl = save_lgl)
}
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local",".GlobalEnv"), function(x) x@lup_tbs_r4)
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4-set
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@lup_tbs_r4 <- value
methods::validObject(x)
x})
#' merge_with_chr_vec<-
#' @name merge_with_chr_vec<--ready4_sp_local
#' @description Set the value of the slot merge_with_chr_vec for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname merge_with_chr_vec-set
methods::setMethod("merge_with_chr_vec<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@merge_with_chr_vec <- value
methods::validObject(x)
x})
#' raw_data_dir_chr<-
#' @name raw_data_dir_chr<--ready4_sp_local
#' @description Set the value of the slot raw_data_dir_chr for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname raw_data_dir_chr-set
methods::setMethod("raw_data_dir_chr<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@raw_data_dir_chr <- value
methods::validObject(x)
x})
#' pckg_chr<-
#' @name pckg_chr<--ready4_sp_local
#' @description Set the value of the slot pckg_chr for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pckg_chr-set
methods::setMethod("pckg_chr<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@pckg_chr <- value
methods::validObject(x)
x})
#' overwrite_lgl<-
#' @name overwrite_lgl<--ready4_sp_local
#' @description Set the value of the slot overwrite_lgl for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname overwrite_lgl-set
methods::setMethod("overwrite_lgl<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@overwrite_lgl <- value
methods::validObject(x)
x})
#' save_lgl<-
#' @name save_lgl<--ready4_sp_local
#' @description Set the value of the slot save_lgl for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_lgl-set
methods::setMethod("save_lgl<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@save_lgl <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_sp_local",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
