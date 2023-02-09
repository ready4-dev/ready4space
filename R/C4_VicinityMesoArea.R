#' VicinityMesoArea
#' 
#' Meso level context - area
#' 
#' @include C4_VicinityMesoRegion.R
#' @slot area_type_chr Area type (a character vector)
#' @slot area_chr Area (a character vector)
#' @slot area_bndy_yr_dbl Area boundary year (a double vector)
#' @slot region_type_chr Region type (a character vector)
#' @slot region_chr Region (a character vector)
#' @slot region_bndy_yr_dbl Region boundary year (a double vector)
#' @slot global_region_chr Global region (a character vector)
#' @slot country_chr Country (a character vector)
#' @slot country_bndy_yr_dbl Country boundary year (a double vector)
#' @slot lookup_r3 Lookup (a ready4 S3)
#' @slot crs_dbl Crs (a double vector)
#' @slot temporal_min_dtm Temporal minimum (a date vector)
#' @slot temporal_max_dtm Temporal maximum (a date vector)
#' @name VicinityMesoArea-class
#' @rdname VicinityMesoArea-class
#' @export VicinityMesoArea
#' @exportClass VicinityMesoArea
VicinityMesoArea <- methods::setClass("VicinityMesoArea",
contains = "VicinityMesoRegion",
slots = c(area_type_chr = "character",area_chr = "character",area_bndy_yr_dbl = "numeric",region_type_chr = "character",region_chr = "character",region_bndy_yr_dbl = "numeric",global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",lookup_r3 = "VicinityLookup",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt"),
prototype =  list(area_type_chr = NA_character_,area_chr = NA_character_,area_bndy_yr_dbl = NA_real_))


methods::setValidity(methods::className("VicinityMesoArea"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
