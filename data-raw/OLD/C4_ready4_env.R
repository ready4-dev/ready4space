#' ready4_env
#' @name ready4_env
#' @description An S4 class to represent Spatiotemporal environment
#' @slot st_data list
#' @slot env_sf sf
#' @slot param_vals tbl_df
ready4_env <- methods::setClass("ready4_env",
slots = c(st_data = "list",env_sf = "sf",param_vals = "tbl_df"),
prototype =  list(st_data = list(list()),env_sf = sf::st_sf(sf::st_sfc()),param_vals = tibble::tibble()))


methods::setValidity(methods::className("ready4_env"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
