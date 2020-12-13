#' ready4_sp_local_proc
#' @name ready4_sp_local_proc
#' @description An S4 class to represent Object defining data to be saved in local directory in a processed (R) format.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot save_type character
#' @slot merge_with_chr_vec character
#' @import ready4use
ready4_sp_local_proc <- methods::setClass("ready4_sp_local_proc",
contains = "ready4_local_proc",
slots = c(lup_tbs_r4 = "ready4_lookup",save_type = "character",merge_with_chr_vec = "character"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))


methods::setValidity(methods::className("ready4_sp_local_proc"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
