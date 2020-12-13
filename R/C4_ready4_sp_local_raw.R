#' ready4_sp_local_raw
#' @name ready4_sp_local_raw
#' @description An S4 class to represent Object defining data to be saved in local directory in a raw (unprocessed) format.
#' @slot lup_tbs_r4 ready4_lookup
#' @slot save_type character
#' @slot merge_with_chr_vec character
#' @import ready4use
ready4_sp_local_raw <- methods::setClass("ready4_sp_local_raw",
contains = "ready4_local_raw",
slots = c(lup_tbs_r4 = "ready4_lookup",save_type = "character",merge_with_chr_vec = "character"),
prototype =  list(lup_tbs_r4 = ready4_lookup()))


methods::setValidity(methods::className("ready4_sp_local_raw"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
