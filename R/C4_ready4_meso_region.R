#' ready4_meso_region
#' @name ready4_meso_region
#' @description An S4 class to represent Meso level context - region
#' @include C4_ready4_macro.R
#' @slot region_type character
#' @slot global_region character
ready4_meso_region <- methods::setClass("ready4_meso_region",
contains = "ready4_macro",
slots = c(region_type = "character"),
prototype =  list(region_type = NA_character_))


methods::setValidity(methods::className("ready4_meso_region",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
