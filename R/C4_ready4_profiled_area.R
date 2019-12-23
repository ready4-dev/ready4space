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
#' country
#' @name country-ready4_profiled_area
#' @description Get the value of the slot country for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country
methods::setMethod("country", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@country)
#' country<-
#' @name country<--ready4_profiled_area
#' @description Set the value of the slot country for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname country-set
methods::setMethod("country<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@country <- value
methods::validObject(x)
x})
#' area_type
#' @name area_type-ready4_profiled_area
#' @description Get the value of the slot area_type for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_type
methods::setMethod("area_type", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@area_type)
#' area_type<-
#' @name area_type<--ready4_profiled_area
#' @description Set the value of the slot area_type for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_type-set
methods::setMethod("area_type<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@area_type <- value
methods::validObject(x)
x})
#' area_bound_year
#' @name area_bound_year-ready4_profiled_area
#' @description Get the value of the slot area_bound_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_bound_year
methods::setMethod("area_bound_year", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@area_bound_year)
#' area_bound_year<-
#' @name area_bound_year<--ready4_profiled_area
#' @description Set the value of the slot area_bound_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname area_bound_year-set
methods::setMethod("area_bound_year<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@area_bound_year <- value
methods::validObject(x)
x})
#' features
#' @name features-ready4_profiled_area
#' @description Get the value of the slot features for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname features
methods::setMethod("features", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@features)
#' features<-
#' @name features<--ready4_profiled_area
#' @description Set the value of the slot features for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname features-set
methods::setMethod("features<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@features <- value
methods::validObject(x)
x})
#' use_coord_lup
#' @name use_coord_lup-ready4_profiled_area
#' @description Get the value of the slot use_coord_lup for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname use_coord_lup
methods::setMethod("use_coord_lup", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@use_coord_lup)
#' use_coord_lup<-
#' @name use_coord_lup<--ready4_profiled_area
#' @description Set the value of the slot use_coord_lup for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname use_coord_lup-set
methods::setMethod("use_coord_lup<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@use_coord_lup <- value
methods::validObject(x)
x})
#' lookup_tb
#' @name lookup_tb-ready4_profiled_area
#' @description Get the value of the slot lookup_tb for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lookup_tb
methods::setMethod("lookup_tb", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@lookup_tb)
#' lookup_tb<-
#' @name lookup_tb<--ready4_profiled_area
#' @description Set the value of the slot lookup_tb for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lookup_tb-set
methods::setMethod("lookup_tb<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@lookup_tb <- value
methods::validObject(x)
x})
#' crs_nbr
#' @name crs_nbr-ready4_profiled_area
#' @description Get the value of the slot crs_nbr for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname crs_nbr
methods::setMethod("crs_nbr", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@crs_nbr)
#' crs_nbr<-
#' @name crs_nbr<--ready4_profiled_area
#' @description Set the value of the slot crs_nbr for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname crs_nbr-set
methods::setMethod("crs_nbr<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@crs_nbr <- value
methods::validObject(x)
x})
#' geom_dist_limit_km
#' @name geom_dist_limit_km-ready4_profiled_area
#' @description Get the value of the slot geom_dist_limit_km for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geom_dist_limit_km
methods::setMethod("geom_dist_limit_km", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@geom_dist_limit_km)
#' geom_dist_limit_km<-
#' @name geom_dist_limit_km<--ready4_profiled_area
#' @description Set the value of the slot geom_dist_limit_km for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname geom_dist_limit_km-set
methods::setMethod("geom_dist_limit_km<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@geom_dist_limit_km <- value
methods::validObject(x)
x})
#' drive_time_limit_mins
#' @name drive_time_limit_mins-ready4_profiled_area
#' @description Get the value of the slot drive_time_limit_mins for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname drive_time_limit_mins
methods::setMethod("drive_time_limit_mins", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@drive_time_limit_mins)
#' drive_time_limit_mins<-
#' @name drive_time_limit_mins<--ready4_profiled_area
#' @description Set the value of the slot drive_time_limit_mins for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname drive_time_limit_mins-set
methods::setMethod("drive_time_limit_mins<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@drive_time_limit_mins <- value
methods::validObject(x)
x})
#' nbr_bands
#' @name nbr_bands-ready4_profiled_area
#' @description Get the value of the slot nbr_bands for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nbr_bands
methods::setMethod("nbr_bands", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@nbr_bands)
#' nbr_bands<-
#' @name nbr_bands<--ready4_profiled_area
#' @description Set the value of the slot nbr_bands for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname nbr_bands-set
methods::setMethod("nbr_bands<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@nbr_bands <- value
methods::validObject(x)
x})
#' data_year
#' @name data_year-ready4_profiled_area
#' @description Get the value of the slot data_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_year
methods::setMethod("data_year", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@data_year)
#' data_year<-
#' @name data_year<--ready4_profiled_area
#' @description Set the value of the slot data_year for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_year-set
methods::setMethod("data_year<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@data_year <- value
methods::validObject(x)
x})
#' data_ymds
#' @name data_ymds-ready4_profiled_area
#' @description Get the value of the slot data_ymds for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_ymds
methods::setMethod("data_ymds", methods::className("ready4_profiled_area",".GlobalEnv"), function(x) x@data_ymds)
#' data_ymds<-
#' @name data_ymds<--ready4_profiled_area
#' @description Set the value of the slot data_ymds for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname data_ymds-set
methods::setMethod("data_ymds<-", methods::className("ready4_profiled_area",".GlobalEnv"), function(x, value) {
x@data_ymds <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_profiled_area",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
