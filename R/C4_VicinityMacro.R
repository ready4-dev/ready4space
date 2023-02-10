#' VicinityMacro
#' 
#' Macro level context
#' 
#' @include C4_VicinityLookup.R
#' @slot a_VicinityLookup  (an instance of the VicinityLookup class)
#' @slot global_region_chr Global region (a character vector)
#' @slot country_chr Country (a character vector)
#' @slot country_bndy_yr_dbl Country boundary year (a double vector)
#' @slot crs_dbl Crs (a double vector)
#' @slot temporal_min_dtm Temporal minimum (a date vector)
#' @slot temporal_max_dtm Temporal maximum (a date vector)
#' @name VicinityMacro-class
#' @rdname VicinityMacro-class
#' @export VicinityMacro
#' @exportClass VicinityMacro
VicinityMacro <- methods::setClass("VicinityMacro",
slots = c(a_VicinityLookup = "VicinityLookup",global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt"),
prototype =  list(a_VicinityLookup = VicinityLookup(),global_region_chr = NA_character_,country_chr = NA_character_,country_bndy_yr_dbl = NA_real_,crs_dbl = NA_real_,temporal_min_dtm = .POSIXct(NA_character_),temporal_max_dtm = .POSIXct(NA_character_)))


methods::setValidity(methods::className("VicinityMacro"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
