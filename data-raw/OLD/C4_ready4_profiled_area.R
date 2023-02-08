#' ready4_profiled_area
#' @name ready4_profiled_area
#' @description An S4 class to represent Information to create a profiled area object
#' @include C4_ready4_micro.R
#' @slot features character
#' @slot use_coord_lup logical
#' @slot geom_dist_limit_km numeric
#' @slot drive_time_limit_mins numeric
#' @slot nbr_bands numeric
#' @slot data_year character
#' @slot data_ymds POSIXt
#' @slot geom_dist_km_cuts numeric
#' @slot travel_time_mins_cuts numeric
#' @slot travel_mode character
#' @slot area_type character
#' @slot area character
#' @slot area_bound_year numeric
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
ready4_profiled_area <- methods::setClass("ready4_profiled_area",
contains = "ready4_micro",
slots = c(features = "character",use_coord_lup = "logical",geom_dist_limit_km = "numeric",drive_time_limit_mins = "numeric",nbr_bands = "numeric",data_year = "character",data_ymds = "POSIXt",geom_dist_km_cuts = "numeric",travel_time_mins_cuts = "numeric",travel_mode = "character",area_type = "character",area = "character",area_bound_year = "numeric",region_type = "character",region = "character",region_bound_year = "numeric",global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "ready4_lookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(features = NA_character_,use_coord_lup = NA,geom_dist_limit_km = NA_real_,drive_time_limit_mins = NA_real_,nbr_bands = NA_real_,data_year = NA_character_,data_ymds = .POSIXct(NA_character_)))


methods::setValidity(methods::className("ready4_profiled_area"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
