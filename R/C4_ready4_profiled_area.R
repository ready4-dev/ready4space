#' ready4_profiled_area
#' @name ready4_profiled_area
#' @description An S4 class to represent Information to create a profiled area object
#' @slot country character
#' @slot area_type character
#' @slot area_bound_year numeric
#' @slot features character
#' @slot use_coord_lup logical
#' @slot lookup_tb ready4_lookup
#' @slot crs_nbr numeric
#' @slot geom_dist_limit_km numeric
#' @slot drive_time_limit_mins numeric
#' @slot nbr_bands numeric
#' @slot data_year character
#' @slot data_ymds POSIXt
methods::setClass(methods::className("ready4_profiled_area",".GlobalEnv"),
slots = c(country = "character",area_type = "character",area_bound_year = "numeric",features = "character",use_coord_lup = "logical",lookup_tb = "ready4_lookup",crs_nbr = "numeric",geom_dist_limit_km = "numeric",drive_time_limit_mins = "numeric",nbr_bands = "numeric",data_year = "character",data_ymds = "POSIXt"),
prototype =  list(country = NA_character_,area_type = NA_character_,area_bound_year = NA_real_,features = NA_character_,use_coord_lup = NA,lookup_tb = ready4_lookup(),crs_nbr = NA_real_,geom_dist_limit_km = NA_real_,drive_time_limit_mins = NA_real_,nbr_bands = NA_real_,data_year = NA_character_,data_ymds = .POSIXct(NA_character_)))

#' ready4_profiled_area
#' @name ready4_profiled_area
#' @description Create a new S4 object of the class:ready4_profiled_area
#' @param country character, Default: 'NA'
#' @param area_type character, Default: 'NA'
#' @param area_bound_year numeric, Default: NA
#' @param features character, Default: 'NA'
#' @param use_coord_lup logical, Default: NA
#' @param lookup_tb ready4_lookup, Default: ready4_lookup()
#' @param crs_nbr numeric, Default: NA
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
ready4_profiled_area <- function(country = NA_character_,
area_type = NA_character_,
area_bound_year = NA_real_,
features = NA_character_,
use_coord_lup = NA,
lookup_tb = ready4_lookup(),
crs_nbr = NA_real_,
geom_dist_limit_km = NA_real_,
drive_time_limit_mins = NA_real_,
nbr_bands = NA_real_,
data_year = NA_character_,
data_ymds = .POSIXct(NA_character_)){ 
methods::new("ready4_profiled_area",
country = country,
area_type = area_type,
area_bound_year = area_bound_year,
features = features,
use_coord_lup = use_coord_lup,
lookup_tb = lookup_tb,
crs_nbr = crs_nbr,
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
