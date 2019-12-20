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
