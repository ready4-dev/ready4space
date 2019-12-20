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

methods::setValidity(methods::className("ready4_sp_local",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
