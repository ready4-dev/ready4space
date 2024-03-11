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
#' @slot a_VicinityLookup  (an instance of the VicinityLookup class)
#' @slot global_region_chr Global region (a character vector)
#' @slot country_chr Country (a character vector)
#' @slot country_bndy_yr_dbl Country boundary year (a double vector)
#' @slot crs_dbl Coordinates reference system (a double vector)
#' @slot temporal_min_dtm Temporal minimum (a date vector)
#' @slot temporal_max_dtm Temporal maximum (a date vector)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @name VicinityMesoArea-class
#' @rdname VicinityMesoArea-class
#' @export VicinityMesoArea
#' @exportClass VicinityMesoArea
VicinityMesoArea <- methods::setClass("VicinityMesoArea",
contains = "VicinityMesoRegion",
slots = c(area_type_chr = "character",area_chr = "character",area_bndy_yr_dbl = "numeric",region_type_chr = "character",region_chr = "character",region_bndy_yr_dbl = "numeric",a_VicinityLookup = "VicinityLookup",global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt",dissemination_1L_chr = "character"),
prototype =  list(area_type_chr = NA_character_,area_chr = NA_character_,area_bndy_yr_dbl = NA_real_))


methods::setValidity(methods::className("VicinityMesoArea"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
