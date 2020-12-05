#' lup_tbs_r4
#' @description S4 Generic function to get the value of the slot lup_tbs_r4
#' @name lup_tbs_r4
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("lup_tbs_r4", function(x) standardGeneric("lup_tbs_r4"))
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local",".GlobalEnv"), function(x) x@lup_tbs_r4)
#' lup_tbs_r4<-
#' @description S4 Generic function to set the value of the slot lup_tbs_r4
#' @name lup_tbs_r4<-
#' @param x An object 
#' 
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @export

methods::setGeneric("lup_tbs_r4<-", function(x, value) standardGeneric("lup_tbs_r4<-"))
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local
#' @param x An object of class ready4_sp_local
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4-set
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local",".GlobalEnv"), function(x, value) {
x@lup_tbs_r4 <- value
methods::validObject(x)
x})
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local_raw
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x) x@lup_tbs_r4)
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local_raw
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_raw
#' @param x An object of class ready4_sp_local_raw
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4-set
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local_raw",".GlobalEnv"), function(x, value) {
x@lup_tbs_r4 <- value
methods::validObject(x)
x})
#' lup_tbs_r4
#' @name lup_tbs_r4-ready4_sp_local_proc
#' @description Get the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4
methods::setMethod("lup_tbs_r4", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x) x@lup_tbs_r4)
#' lup_tbs_r4<-
#' @name lup_tbs_r4<--ready4_sp_local_proc
#' @description Set the value of the slot lup_tbs_r4 for S4 objects of class ready4_sp_local_proc
#' @param x An object of class ready4_sp_local_proc
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname lup_tbs_r4-set
methods::setMethod("lup_tbs_r4<-", methods::className("ready4_sp_local_proc",".GlobalEnv"), function(x, value) {
x@lup_tbs_r4 <- value
methods::validObject(x)
x})
