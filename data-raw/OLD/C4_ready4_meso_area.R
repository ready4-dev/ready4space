#' VicinityMesoArea
#' @name VicinityMesoArea
#' @description An S4 class to represent Meso level context - area
#' @include C4_VicinityMesoRegion.R
#' @slot area_type character
#' @slot area character
#' @slot area_bound_year numeric
#' @slot region_type character
#' @slot region character
#' @slot region_bound_year numeric
#' @slot global_region character
#' @slot country character
#' @slot country_bound_year numeric
#' @slot lookup_tb VicinityLookup
#' @slot crs_nbr numeric
#' @slot temporal_min POSIXt
#' @slot temporal_max POSIXt
VicinityMesoArea <- methods::setClass("VicinityMesoArea",
contains = "VicinityMesoRegion",
slots = c(area_type = "character",area = "character",area_bound_year = "numeric",region_type = "character",region = "character",region_bound_year = "numeric",global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "VicinityLookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(area_type = NA_character_,area = NA_character_,area_bound_year = NA_real_))


methods::setValidity(methods::className("VicinityMesoArea"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
