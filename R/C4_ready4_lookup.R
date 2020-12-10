#' ready4_lookup
#' @name ready4_lookup
#' @description An S4 class to represent Look up tables to use throughout ready4 suite
setOldClass(c("ready4_sp_abbreviations_lup","tbl_df", "tbl", "data.frame"))
#' @slot sp_abbreviations_lup ready4_sp_abbreviations_lup
ready4_lookup <- methods::setClass("ready4_lookup",
slots = c(sp_abbreviations_lup = "ready4_sp_abbreviations_lup"),
prototype =  list(sp_abbreviations_lup = "raw"))


methods::setValidity(methods::className("ready4_lookup",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
