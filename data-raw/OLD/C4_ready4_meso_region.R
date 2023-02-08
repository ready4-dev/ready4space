#' ready4_meso_region
#' @name ready4_meso_region
#' @description An S4 class to represent Meso level context - region
#' @include C4_ready4_macro.R
#' @slot region_type character
#' @slot region character
#' @slot region_bound_year numeric
#' @slot global_region character
#' @slot country character
#' @slot country_bound_year numeric
#' @slot lookup_tb ready4_lookup
#' @slot crs_nbr numeric
#' @slot temporal_min POSIXt
#' @slot temporal_max POSIXt
ready4_meso_region <- methods::setClass("ready4_meso_region",
contains = "ready4_macro",
slots = c(region_type = "character",region = "character",region_bound_year = "numeric",global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "ready4_lookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(region_type = NA_character_,region = NA_character_,region_bound_year = NA_real_))


methods::setValidity(methods::className("ready4_meso_region"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
