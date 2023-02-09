#' VicinityLocalpRaw
#' 
#' Object defining data to be saved in local directory in a raw (unprocessed) format.
#' 
#' @include C4_VicinityLookup.R
#' @slot lup_tbs_r4 Lookup table tibbles (a ready4 S4)
#' @slot write_type_1L_chr Write type (a character vector of length one)
#' @slot merge_itms_chr Merge items (a character vector)
#' @slot raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @slot pkg_1L_chr Package (a character vector of length one)
#' @slot overwrite_1L_lgl Overwrite (a logical vector of length one)
#' @slot write_1L_lgl Write (a logical vector of length one)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4use
#' @name VicinityLocalpRaw-class
#' @rdname VicinityLocalpRaw-class
#' @export VicinityLocalpRaw
#' @exportClass VicinityLocalpRaw
VicinityLocalpRaw <- methods::setClass("VicinityLocalpRaw",
contains = "Ready4useRaw",
slots = c(lup_tbs_r4 = "VicinityLookup",write_type_1L_chr = "character",merge_itms_chr = "character",raw_fls_dir_1L_chr = "character",pkg_1L_chr = "character",overwrite_1L_lgl = "logical",write_1L_lgl = "logical",dissemination_1L_chr = "character"),
prototype =  list(lup_tbs_r4 = VicinityLookup()))


methods::setValidity(methods::className("VicinityLocalpRaw"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
