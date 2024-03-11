#' VicinityMesoRegion
#' 
#' Meso level context - region
#' 
#' @include C4_VicinityMacro.R
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
#' @name VicinityMesoRegion-class
#' @rdname VicinityMesoRegion-class
#' @export VicinityMesoRegion
#' @exportClass VicinityMesoRegion
VicinityMesoRegion <- methods::setClass("VicinityMesoRegion",
contains = "VicinityMacro",
slots = c(region_type_chr = "character",region_chr = "character",region_bndy_yr_dbl = "numeric",a_VicinityLookup = "VicinityLookup",global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt",dissemination_1L_chr = "character"),
prototype =  list(region_type_chr = NA_character_,region_chr = NA_character_,region_bndy_yr_dbl = NA_real_))


methods::setValidity(methods::className("VicinityMesoRegion"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
