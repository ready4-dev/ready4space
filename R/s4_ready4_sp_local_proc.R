#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local_proc
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x) x@lup_tbs_r4)
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local_proc
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4-set
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@lup_tbs_r4 <- value
methods::validObject(x)
x})
#' save_type<-
#' @name save_type<--ready4_sp_local_proc
#' @description Set the value of the slot save_type for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_type-set
methods::setMethod("save_type<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@save_type <- value
methods::validObject(x)
x})
#' proc_data_dir_chr<-
#' @name proc_data_dir_chr<--ready4_sp_local_proc
#' @description Set the value of the slot proc_data_dir_chr for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname proc_data_dir_chr-set
methods::setMethod("proc_data_dir_chr<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@proc_data_dir_chr <- value
methods::validObject(x)
x})
#' import_chr_vec<-
#' @name import_chr_vec<--ready4_sp_local_proc
#' @description Set the value of the slot import_chr_vec for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname import_chr_vec-set
methods::setMethod("import_chr_vec<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@import_chr_vec <- value
methods::validObject(x)
x})
#' path_to_starter_sf_chr<-
#' @name path_to_starter_sf_chr<--ready4_sp_local_proc
#' @description Set the value of the slot path_to_starter_sf_chr for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname path_to_starter_sf_chr-set
methods::setMethod("path_to_starter_sf_chr<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@path_to_starter_sf_chr <- value
methods::validObject(x)
x})
#' import_this_ls<-
#' @name import_this_ls<--ready4_sp_local_proc
#' @description Set the value of the slot import_this_ls for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname import_this_ls-set
methods::setMethod("import_this_ls<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@import_this_ls <- value
methods::validObject(x)
x})
#' merge_with_chr_vec<-
#' @name merge_with_chr_vec<--ready4_sp_local_proc
#' @description Set the value of the slot merge_with_chr_vec for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname merge_with_chr_vec-set
methods::setMethod("merge_with_chr_vec<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@merge_with_chr_vec <- value
methods::validObject(x)
x})
#' raw_data_dir_chr<-
#' @name raw_data_dir_chr<--ready4_sp_local_proc
#' @description Set the value of the slot raw_data_dir_chr for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname raw_data_dir_chr-set
methods::setMethod("raw_data_dir_chr<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@raw_data_dir_chr <- value
methods::validObject(x)
x})
#' pckg_chr<-
#' @name pckg_chr<--ready4_sp_local_proc
#' @description Set the value of the slot pckg_chr for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pckg_chr-set
methods::setMethod("pckg_chr<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@pckg_chr <- value
methods::validObject(x)
x})
#' overwrite_lgl<-
#' @name overwrite_lgl<--ready4_sp_local_proc
#' @description Set the value of the slot overwrite_lgl for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname overwrite_lgl-set
methods::setMethod("overwrite_lgl<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@overwrite_lgl <- value
methods::validObject(x)
x})
#' save_lgl<-
#' @name save_lgl<--ready4_sp_local_proc
#' @description Set the value of the slot save_lgl for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_lgl-set
methods::setMethod("save_lgl<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@save_lgl <- value
methods::validObject(x)
x})
