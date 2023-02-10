#' VicinityLocalRaw
#' @name VicinityLocalRaw
#' @description An S4 class to represent Object defining data to be saved in local directory in a raw (unprocessed) format.
#' @slot lup_tbs_r4 VicinityLookup
#' @slot write_type_1L_chr character
#' @slot merge_itms_chr character
#' @import ready4use
VicinityLocalRaw <- methods::setClass("VicinityLocalRaw",
contains = "Ready4useRaw",
slots = c(lup_tbs_r4 = "VicinityLookup",write_type_1L_chr = "character",merge_itms_chr = "character"),
prototype =  list(lup_tbs_r4 = VicinityLookup()))


methods::setValidity(methods::className("VicinityLocalRaw"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
