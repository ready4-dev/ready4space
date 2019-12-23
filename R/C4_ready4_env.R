#' ready4_env
#' @name ready4_env
#' @description An S4 class to represent Spatiotemporal environment
#' @slot st_data list
#' @slot env_sf sf
#' @slot par_vals tbl_df
methods::setClass(methods::className("ready4_env",".GlobalEnv"),
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
#' st_data
#' @name st_data-ready4_env
#' @description Get the value of the slot st_data for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname st_data
methods::setMethod("st_data", methods::className("ready4_env",".GlobalEnv"), function(x) x@st_data)
#' st_data<-
#' @name st_data<--ready4_env
#' @description Set the value of the slot st_data for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname st_data-set
methods::setMethod("st_data<-", methods::className("ready4_env",".GlobalEnv"), function(x, value) {
x@st_data <- value
methods::validObject(x)
x})
#' env_sf
#' @name env_sf-ready4_env
#' @description Get the value of the slot env_sf for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname env_sf
methods::setMethod("env_sf", methods::className("ready4_env",".GlobalEnv"), function(x) x@env_sf)
#' env_sf<-
#' @name env_sf<--ready4_env
#' @description Set the value of the slot env_sf for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname env_sf-set
methods::setMethod("env_sf<-", methods::className("ready4_env",".GlobalEnv"), function(x, value) {
x@env_sf <- value
methods::validObject(x)
x})
#' par_vals
#' @name par_vals-ready4_env
#' @description Get the value of the slot par_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname par_vals
methods::setMethod("par_vals", methods::className("ready4_env",".GlobalEnv"), function(x) x@par_vals)
#' par_vals<-
#' @name par_vals<--ready4_env
#' @description Set the value of the slot par_vals for S4 objects of class ready4_env
#' @param x An object of class ready4_env
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname par_vals-set
methods::setMethod("par_vals<-", methods::className("ready4_env",".GlobalEnv"), function(x, value) {
x@par_vals <- value
methods::validObject(x)
x})

methods::setValidity(methods::className("ready4_env",".GlobalEnv"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
