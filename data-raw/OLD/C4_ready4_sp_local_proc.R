#' ready4_spProcessed
#' @name ready4_spProcessed
#' @description An S4 class to represent Object defining data to be saved in local directory in a processed (R) format.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot write_type_1L_chr character
#' @slot merge_itms_chr character
#' @import ready4use
ready4_spProcessed <- methods::setClass("ready4_spProcessed",
contains = "Ready4useProcessed",
slots = c(lup_tbs_r4 = "ready4_lookup",write_type_1L_chr = "character",merge_itms_chr = "character"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))


methods::setValidity(methods::className("ready4_spProcessed"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
