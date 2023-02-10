#' VicinityLocalProcessed
#' @name VicinityLocalProcessed
#' @description An S4 class to represent Object defining data to be saved in local directory in a processed (R) format.
#' @slot lup_tbs_r4 VicinityLookup
#' @slot write_type_1L_chr character
#' @slot merge_itms_chr character
#' @import ready4use
VicinityLocalProcessed <- methods::setClass("VicinityLocalProcessed",
contains = "Ready4useProcessed",
slots = c(lup_tbs_r4 = "VicinityLookup",write_type_1L_chr = "character",merge_itms_chr = "character"),
prototype =  list(lup_tbs_r4 = VicinityLookup()))


methods::setValidity(methods::className("VicinityLocalProcessed"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
