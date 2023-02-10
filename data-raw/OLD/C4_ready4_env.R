#' VicinitySpaceTime
#' @name VicinitySpaceTime
#' @description An S4 class to represent Spatiotemporal environment
#' @slot st_data list
#' @slot env_sf sf
#' @slot param_vals tbl_df
VicinitySpaceTime <- methods::setClass("VicinitySpaceTime",
slots = c(st_data = "list",env_sf = "sf",param_vals = "tbl_df"),
prototype =  list(st_data = list(list()),env_sf = sf::st_sf(sf::st_sfc()),param_vals = tibble::tibble()))


methods::setValidity(methods::className("VicinitySpaceTime"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
