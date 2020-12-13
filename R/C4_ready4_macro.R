#' ready4_macro
#' @name ready4_macro
#' @description An S4 class to represent Macro level context
#' @slot global_region character
#' @slot country character
#' @slot country_bound_year numeric
#' @slot lookup_tb ready4_lookup
#' @slot crs_nbr numeric
#' @slot temporal_min POSIXt
#' @slot temporal_max POSIXt
ready4_macro <- methods::setClass("ready4_macro",
slots = c(global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "ready4_lookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(global_region = NA_character_,country = NA_character_,country_bound_year = NA_real_,lookup_tb = ready4_lookup(),crs_nbr = NA_real_,temporal_min = .POSIXct(NA_character_),temporal_max = .POSIXct(NA_character_)))


methods::setValidity(methods::className("ready4_macro"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
