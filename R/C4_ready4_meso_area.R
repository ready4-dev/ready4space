#' ready4_meso_area
#' @name ready4_meso_area
#' @description An S4 class to represent Meso level context - area
#' @include C4_ready4_meso_region.R
#' @slot area_type character
#' @slot region_type character
#' @slot global_region character
ready4_meso_area <- methods::setClass("ready4_meso_area",
contains = "ready4_meso_region",
slots = c(area_type = "character"),
prototype =  list(area_type = NA_character_))


methods::setValidity(methods::className("ready4_meso_area",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
