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
