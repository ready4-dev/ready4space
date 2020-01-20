#' ready4_macro
#' @name ready4_macro
#' @description An S4 class to represent Macro level context
#' @slot global_region character
#' @slot country character
#' @slot country_bound_year numeric
#' @slot lookup_tb ready4_lookup
#' @slot crs_nbr numeric
#' @slot temporal_min POSIXt
#' @slot temporal_max POSIXt
methods::setClass("ready4_macro",
slots = c(global_region = "character",country = "character",country_bound_year = "numeric",lookup_tb = "ready4_lookup",crs_nbr = "numeric",temporal_min = "POSIXt",temporal_max = "POSIXt"),
prototype =  list(global_region = NA_character_,country = NA_character_,country_bound_year = NA_real_,lookup_tb = ready4_lookup(),crs_nbr = NA_real_,temporal_min = .POSIXct(NA_character_),temporal_max = .POSIXct(NA_character_)))

#' ready4_macro
#' @name ready4_macro
#' @description Create a new S4 object of the class:ready4_macro
#' @param global_region character, Default: 'NA'
#' @param country character, Default: 'NA'
#' @param country_bound_year numeric, Default: NA
#' @param lookup_tb ready4_lookup, Default: ready4_lookup()
#' @param crs_nbr numeric, Default: NA
#' @param temporal_min POSIXt, Default: .POSIXct(NA_character_)
#' @param temporal_max POSIXt, Default: .POSIXct(NA_character_)
#' @return An S4 object of the ready4_macro class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[methods]{new}}
#' @rdname ready4_macro
#' @export 
#' @importFrom methods new
ready4_macro <- function(global_region = NA_character_,
country = NA_character_,
country_bound_year = NA_real_,
lookup_tb = ready4_lookup(),
crs_nbr = NA_real_,
temporal_min = .POSIXct(NA_character_),
temporal_max = .POSIXct(NA_character_)){ 
methods::new("ready4_macro",
global_region = global_region,
country = country,
country_bound_year = country_bound_year,
lookup_tb = lookup_tb,
crs_nbr = crs_nbr,
temporal_min = temporal_min,
temporal_max = temporal_max)
}

methods::setValidity(methods::className("ready4_macro",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
