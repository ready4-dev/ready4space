#' VicinityMicro
#' 
#' Micro level context
#' 
#' @include C4_VicinityMesoArea.R
#' @slot geom_dist_km_cuts_dbl Geometry distance kilometre cuts (a double vector)
#' @slot travel_time_mins_cuts_dbl Travel time minimums cuts (a double vector)
#' @slot travel_mode_chr Travel mode (a character vector)
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
#' @name VicinityMicro-class
#' @rdname VicinityMicro-class
#' @export VicinityMicro
#' @exportClass VicinityMicro
VicinityMicro <- methods::setClass("VicinityMicro",
contains = "VicinityMesoArea",
slots = c(geom_dist_km_cuts_dbl = "numeric",travel_time_mins_cuts_dbl = "numeric",travel_mode_chr = "character",area_type_chr = "character",area_chr = "character",area_bndy_yr_dbl = "numeric",region_type_chr = "character",region_chr = "character",region_bndy_yr_dbl = "numeric",a_VicinityLookup = "VicinityLookup",global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt",dissemination_1L_chr = "character"),
prototype =  list(geom_dist_km_cuts_dbl = NA_real_,travel_time_mins_cuts_dbl = NA_real_,travel_mode_chr = NA_character_))


methods::setValidity(methods::className("VicinityMicro"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
