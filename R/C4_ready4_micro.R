#' ready4_micro
#' @name ready4_micro
#' @description An S4 class to represent Micro level context
#' @include C4_ready4_meso_area.R
#' @slot geom_dist_km_cuts numeric
#' @slot area_type character
#' @slot region_type character
#' @slot global_region character
ready4_micro <- methods::setClass("ready4_micro",
contains = "ready4_meso_area",
slots = c(geom_dist_km_cuts = "numeric"),
prototype =  list(geom_dist_km_cuts = NA_real_))

