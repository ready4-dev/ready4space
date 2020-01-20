#' ready4_sp_local_proc
#' @name ready4_sp_local_proc
#' @description An S4 class to represent Object defining data to be saved in local directory in a processed (R) format.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot save_type character
#' @slot proc_data_dir_chr character
#' @slot import_chr_vec character
#' @slot path_to_starter_sf_chr character
#' @slot import_this_ls list
#' @slot merge_with_chr_vec character
#' @slot raw_data_dir_chr character
#' @slot pckg_chr character
#' @slot overwrite_lgl logical
#' @slot save_lgl logical
#' @import ready4use
methods::setClass("ready4_sp_local_proc",
contains = "ready4_local_proc",
slots = c(lup_tbs_r4 = "ready4_lookup"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))

#' ready4_sp_local_proc
#' @name ready4_sp_local_proc
#' @description Create a new S4 object of the class:ready4_sp_local_proc
#' @param save_type character, Default: 'NA'
#' @param proc_data_dir_chr character, Default: 'NA'
#' @param import_chr_vec character, Default: 'NA'
#' @param path_to_starter_sf_chr character, Default: 'NA'
#' @param import_this_ls list, Default: list(list())
#' @param merge_with_chr_vec character, Default: 'NA'
#' @param raw_data_dir_chr character, Default: 'NA'
#' @param pckg_chr character, Default: 'NA'
#' @param overwrite_lgl logical, Default: NA
#' @param save_lgl logical, Default: NA
#' @param lup_tbs_r4 ready4_lookup, Default: ready4_lookup()
#' @return An S4 object of the ready4_sp_local_proc class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_sp_local_proc
#' @export 
#' @importFrom methods new
ready4_sp_local_proc <- function(save_type = NA_character_,
proc_data_dir_chr = NA_character_,
import_chr_vec = NA_character_,
path_to_starter_sf_chr = NA_character_,
import_this_ls = list(list()),
merge_with_chr_vec = NA_character_,
raw_data_dir_chr = NA_character_,
pckg_chr = NA_character_,
overwrite_lgl = NA,
save_lgl = NA,
lup_tbs_r4 = ready4_lookup()){ 
methods::new("ready4_sp_local_proc",
save_type = save_type,
proc_data_dir_chr = proc_data_dir_chr,
import_chr_vec = import_chr_vec,
path_to_starter_sf_chr = path_to_starter_sf_chr,
import_this_ls = import_this_ls,
merge_with_chr_vec = merge_with_chr_vec,
raw_data_dir_chr = raw_data_dir_chr,
pckg_chr = pckg_chr,
overwrite_lgl = overwrite_lgl,
save_lgl = save_lgl,
lup_tbs_r4 = lup_tbs_r4)
}

methods::setValidity(methods::className("ready4_sp_local_proc",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
