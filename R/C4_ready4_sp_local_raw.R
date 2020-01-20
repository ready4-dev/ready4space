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
methods::setClass("ready4_sp_local_raw",
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
#' @param lup_tbs_r4 ready4_lookup, Default: ready4_lookup()
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
save_lgl = NA,
lup_tbs_r4 = ready4_lookup()){ 
methods::new("ready4_sp_local_raw",
save_type = save_type,
merge_with_chr_vec = merge_with_chr_vec,
raw_data_dir_chr = raw_data_dir_chr,
pckg_chr = pckg_chr,
overwrite_lgl = overwrite_lgl,
save_lgl = save_lgl,
lup_tbs_r4 = lup_tbs_r4)
}

methods::setValidity(methods::className("ready4_sp_local_raw",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
