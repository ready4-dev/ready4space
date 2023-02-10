#' VicinityLocal
#' @name VicinityLocal
#' @description An S4 class to represent Object defining data to be saved in local directory.
#' @slot lup_tbs_r4 VicinityLookup
#' @slot merge_itms_chr character
#' @import ready4use
VicinityLocal <- methods::setClass("VicinityLocal",
contains = "Ready4useFiles",
slots = c(lup_tbs_r4 = "VicinityLookup",merge_itms_chr = "character"),
prototype =  list(lup_tbs_r4 = VicinityLookup()))


methods::setValidity(methods::className("VicinityLocal"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
