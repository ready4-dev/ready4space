#' ready4_macro
#' @name ready4_macro
#' @description An S4 class to represent Macro level context
#' @slot global_region character
ready4_macro <- methods::setClass("ready4_macro",
slots = c(global_region = "character"),
prototype =  list(global_region = NA_character_))


methods::setValidity(methods::className("ready4_macro",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
