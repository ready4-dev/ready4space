#' VicinityMacro
#' @name VicinityMacro
#' @description An S4 class to represent Macro level context
#' @slot global_region character
#' @slot country character
#' @slot country_bound_year numeric
#' @slot lookup_tb VicinityLookup
#' @slot crs_nbr numeric
#' @slot temporal_min POSIXt
#' @slot temporal_max POSIXt
VicinityMacro <- methods::setClass("VicinityMacro",
slots = c(global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "VicinityLookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(global_region = NA_character_,country = NA_character_,country_bound_year = NA_real_,lookup_tb = VicinityLookup(),crs_nbr = NA_real_,temporal_min = .POSIXct(NA_character_),temporal_max = .POSIXct(NA_character_)))


methods::setValidity(methods::className("VicinityMacro"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
