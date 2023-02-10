#' VicinityMesoRegion
#' @name VicinityMesoRegion
#' @description An S4 class to represent Meso level context - region
#' @include C4_VicinityMacro.R
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
VicinityMesoRegion <- methods::setClass("VicinityMesoRegion",
contains = "VicinityMacro",
slots = c(region_type = "character",region = "character",region_bound_year = "numeric",global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "VicinityLookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(region_type = NA_character_,region = NA_character_,region_bound_year = NA_real_))


methods::setValidity(methods::className("VicinityMesoRegion"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
