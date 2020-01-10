#' ready4_micro
#' @name ready4_micro
#' @description An S4 class to represent Micro level context
#' @include C4_ready4_meso_area.R
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
methods::setClass(methods::className("ready4_micro",".GlobalEnv"),
contains = "ready4_meso_area",
slots = c(geom_dist_km_cuts = "numeric",travel_time_mins_cuts = "numeric",travel_mode = "character"),
prototype =  list(geom_dist_km_cuts = NA_real_,travel_time_mins_cuts = NA_real_,travel_mode = NA_character_))

#' ready4_micro
#' @name ready4_micro
#' @description Create a new S4 object of the class:ready4_micro
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
#' @return An S4 object of the ready4_micro class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_micro
#' @export 
#' @importFrom methods new
ready4_micro <- function(area_type = NA_character_,
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
temporal_max = .POSIXct(NA_character_)){ 
methods::new("ready4_micro",
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
temporal_max = temporal_max)
}

methods::setValidity(methods::className("ready4_micro",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
