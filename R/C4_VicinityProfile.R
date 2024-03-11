#' VicinityProfile
#' 
#' Information to create a profiled area object
#' 
#' @include C4_VicinityMicro.R
#' @slot features_chr Features (a character vector)
#' @slot use_coord_lup_lgl Use coord lookup table (a logical vector)
#' @slot geomc_dist_limit_km_dbl Geometric distance limit kilometre (a double vector)
#' @slot drive_time_limit_mins_dbl Drive time limit minimums (a double vector)
#' @slot nbr_bands_dbl Number bands (a double vector)
#' @slot data_year_1L_chr Data year (a character vector of length one)
#' @slot data_ymds_dtm Data ymds (a date vector)
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
#' @name VicinityProfile-class
#' @rdname VicinityProfile-class
#' @export VicinityProfile
#' @exportClass VicinityProfile
VicinityProfile <- methods::setClass("VicinityProfile",
contains = "VicinityMicro",
slots = c(features_chr = "character",use_coord_lup_lgl = "logical",geomc_dist_limit_km_dbl = "numeric",drive_time_limit_mins_dbl = "numeric",nbr_bands_dbl = "numeric",data_year_1L_chr = "character",data_ymds_dtm = "POSIXt",geom_dist_km_cuts_dbl = "numeric",travel_time_mins_cuts_dbl = "numeric",travel_mode_chr = "character",area_type_chr = "character",area_chr = "character",area_bndy_yr_dbl = "numeric",region_type_chr = "character",region_chr = "character",region_bndy_yr_dbl = "numeric",a_VicinityLookup = "VicinityLookup",global_region_chr = "character",country_chr = "character",country_bndy_yr_dbl = "numeric",crs_dbl = "numeric",temporal_min_dtm = "POSIXt",temporal_max_dtm = "POSIXt",dissemination_1L_chr = "character"),
prototype =  list(features_chr = NA_character_,use_coord_lup_lgl = NA,geomc_dist_limit_km_dbl = NA_real_,drive_time_limit_mins_dbl = NA_real_,nbr_bands_dbl = NA_real_,data_year_1L_chr = NA_character_,data_ymds_dtm = .POSIXct(NA_character_)))


methods::setValidity(methods::className("VicinityProfile"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
