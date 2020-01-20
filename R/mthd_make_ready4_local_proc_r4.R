#' Make a ready4_local_proc_r4 object
#' @description Make a ready4_local_proc_r4 object from a range of potential inputs
#' @name make_local_proc_r4
#' @param x Primary input object - see Usage section for allowable signatures
NULL

#' @importMethodsFrom ready4use make_local_proc_r4 import_chr_vec<- save_lgl<- raw_data_dir_chr<-
#' @import ready4use
#' @param raw_data_dir_chr A character
#' @param save_lgl A logical
#' @export
#' @rdname make_local_proc_r4
methods::setMethod("make_local_proc_r4",
                   c("ready4_script_data"), function(x,
                                                     raw_data_dir_chr,
                                                     save_lgl){
                     x %>%
                       #ready4use::`import_chr_vec<-`(import_chr_vec) %>%
                       ready4use::`save_lgl<-`(save_lgl) %>%
                       ready4use::`raw_data_dir_chr<-`(raw_data_dir_chr)
                   })

#' @param import_chr_vec A character
#' @param raw_data_dir_chr A character
#' @param save_lgl A logical
#' @rdname make_local_proc_r4
#' @export
methods::setMethod("make_local_proc_r4",
                   "ready4_sp_local",
                   function(x,
                            import_chr_vec,
                            raw_data_dir_chr,
                            save_lgl){
                     ready4_sp_local_proc(lup_tbs_r4 = x@lup_tbs_r4,
                                          import_chr_vec = import_chr_vec,
                                          merge_with_chr_vec = x@merge_with_chr_vec,
                                          overwrite_lgl = x@overwrite_lgl,
                                          raw_data_dir_chr = raw_data_dir_chr,
                                          save_lgl = save_lgl)
                   })
