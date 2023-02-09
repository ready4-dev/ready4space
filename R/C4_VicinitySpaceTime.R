#' VicinitySpaceTime
#' 
#' Spatiotemporal environment
#' 
#' @slot data_ls Data (a list)
#' @slot env_sf Environment (a simple features object)
#' @slot param_vals_tb Parameter values (a tibble)
#' @name VicinitySpaceTime-class
#' @rdname VicinitySpaceTime-class
#' @export VicinitySpaceTime
#' @exportClass VicinitySpaceTime
VicinitySpaceTime <- methods::setClass("VicinitySpaceTime",
slots = c(data_ls = "list",env_sf = "sf",param_vals_tb = "tbl_df"),
prototype =  list(data_ls = list(list()),env_sf = sf::st_sf(sf::st_sfc()),param_vals_tb = tibble::tibble()))


methods::setValidity(methods::className("VicinitySpaceTime"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
