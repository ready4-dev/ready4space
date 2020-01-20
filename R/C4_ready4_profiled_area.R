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
methods::setClass("ready4_profiled_area",
contains = "ready4_micro",
slots = c(features = "character",use_coord_lup = "logical",geom_dist_limit_km = "numeric",drive_time_limit_mins = "numeric",nbr_bands = "numeric",data_year = "character",data_ymds = "POSIXt"),
prototype =  list(features = NA_character_,use_coord_lup = NA,geom_dist_limit_km = NA_real_,drive_time_limit_mins = NA_real_,nbr_bands = NA_real_,data_year = NA_character_,data_ymds = .POSIXct(NA_character_)))

#' ready4_profiled_area
#' @name ready4_profiled_area
#' @description Create a new S4 object of the class:ready4_profiled_area
#' @param geom_dist_km_cuts numeric, Default: NA
#' @param travel_time_mins_cuts numeric, Default: NA
#' @param travel_mode character, Default: 'NA'
#' @param area_type character, Default: 'NA'
#' @param area character, Default: 'NA'
#' @param area_bound_year numeric, Default: NA
#' @param region_type character, Default: 'NA'
#' @param region character, Default: 'NA'
#' @param region_bound_year numeric, Default: NA
#' @param global_region character, Default: 'NA'
#' @param country character, Default: 'NA'
#' @param country_bound_year numeric, Default: NA
#' @param lookup_tb ready4_lookup, Default: ready4_lookup()
#' @param crs_nbr numeric, Default: NA
#' @param temporal_min POSIXt, Default: .POSIXct(NA_character_)
#' @param temporal_max POSIXt, Default: .POSIXct(NA_character_)
#' @param features character, Default: 'NA'
#' @param use_coord_lup logical, Default: NA
#' @param geom_dist_limit_km numeric, Default: NA
#' @param drive_time_limit_mins numeric, Default: NA
#' @param nbr_bands numeric, Default: NA
#' @param data_year character, Default: 'NA'
#' @param data_ymds POSIXt, Default: .POSIXct(NA_character_)
#' @return An S4 object of the ready4_profiled_area class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_profiled_area
#' @export 
#' @importFrom methods new
ready4_profiled_area <- function(geom_dist_km_cuts = NA_real_,
travel_time_mins_cuts = NA_real_,
travel_mode = NA_character_,
area_type = NA_character_,
area = NA_character_,
area_bound_year = NA_real_,
region_type = NA_character_,
region = NA_character_,
region_bound_year = NA_real_,
global_region = NA_character_,
country = NA_character_,
country_bound_year = NA_real_,
lookup_tb = ready4_lookup(),
crs_nbr = NA_real_,
temporal_min = .POSIXct(NA_character_),
temporal_max = .POSIXct(NA_character_),
features = NA_character_,
use_coord_lup = NA,
geom_dist_limit_km = NA_real_,
drive_time_limit_mins = NA_real_,
nbr_bands = NA_real_,
data_year = NA_character_,
data_ymds = .POSIXct(NA_character_)){ 
methods::new("ready4_profiled_area",
geom_dist_km_cuts = geom_dist_km_cuts,
travel_time_mins_cuts = travel_time_mins_cuts,
travel_mode = travel_mode,
area_type = area_type,
area = area,
area_bound_year = area_bound_year,
region_type = region_type,
region = region,
region_bound_year = region_bound_year,
global_region = global_region,
country = country,
country_bound_year = country_bound_year,
lookup_tb = lookup_tb,
crs_nbr = crs_nbr,
temporal_min = temporal_min,
temporal_max = temporal_max,
features = features,
use_coord_lup = use_coord_lup,
geom_dist_limit_km = geom_dist_limit_km,
drive_time_limit_mins = drive_time_limit_mins,
nbr_bands = nbr_bands,
data_year = data_year,
data_ymds = data_ymds)
}

methods::setValidity(methods::className("ready4_profiled_area",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
