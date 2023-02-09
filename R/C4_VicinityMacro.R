#' VicinityMacro
#' 
#' Macro level context
#' 
#' @include C4_VicinityLookup.R
#' @slot global_region_chr Global region (a character vector)
#' @slot country_chr Country (a character vector)
#' @slot country_bndy_yr_dbl Country boundary year (a double vector)
#' @slot lookup_r3 Lookup (a ready4 S3)
#' @slot crs_dbl Crs (a double vector)
#' @slot temporal_min_dtm Temporal minimum (a date vector)
#' @slot temporal_max_dtm Temporal maximum (a date vector)
#' @name VicinityMacro-class
#' @rdname VicinityMacro-class
#' @export VicinityMacro
#' @exportClass VicinityMacro
VicinityMacro <- methods::setClass("VicinityMacro",
slots = c(global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",lookup_r3 = "VicinityLookup",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt"),
prototype =  list(global_region_chr = NA_character_,country_chr = NA_character_,country_bndy_yr_dbl = NA_real_,lookup_r3 = VicinityLookup(),crs_dbl = NA_real_,temporal_min_dtm = .POSIXct(NA_character_),temporal_max_dtm = .POSIXct(NA_character_)))


methods::setValidity(methods::className("VicinityMacro"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
