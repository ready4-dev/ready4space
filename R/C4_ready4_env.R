#' ready4_env
#' @name ready4_env
#' @description An S4 class to represent Spatiotemporal environment
#' @slot st_data list
#' @slot env_sf sf
#' @slot par_vals tbl_df
methods::setClass("ready4_env",
slots = c(st_data = "list",env_sf = "sf",par_vals = "tbl_df"),
prototype =  list(st_data = list(list()),env_sf = sf::st_sf(sf::st_sfc()),par_vals = tibble::tibble()))

#' ready4_env
#' @name ready4_env
#' @description Create a new S4 object of the class:ready4_env
#' @param st_data list, Default: list(list())
#' @param env_sf sf, Default: sf::st_sf(sf::st_sfc())
#' @param par_vals tbl_df, Default: tibble::tibble()
#' @return An S4 object of the ready4_env class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[sf]{sf}},\code{\link[sf]{sfc}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[methods]{new}}
#' @rdname ready4_env
#' @export 
#' @importFrom sf st_sf st_sfc
#' @importFrom tibble tibble
#' @importFrom methods new
ready4_env <- function(st_data = list(list()),
env_sf = sf::st_sf(sf::st_sfc()),
par_vals = tibble::tibble()){ 
methods::new("ready4_env",
st_data = st_data,
env_sf = env_sf,
par_vals = par_vals)
}

methods::setValidity(methods::className("ready4_env",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
