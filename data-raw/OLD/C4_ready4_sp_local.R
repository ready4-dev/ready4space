#' ready4_sp_local
#' @name ready4_sp_local
#' @description An S4 class to represent Object defining data to be saved in local directory.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot merge_itms_chr character
#' @import ready4use
ready4_sp_local <- methods::setClass("ready4_sp_local",
contains = "Ready4useFiles",
slots = c(lup_tbs_r4 = "ready4_lookup",merge_itms_chr = "character"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))


methods::setValidity(methods::className("ready4_sp_local"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
