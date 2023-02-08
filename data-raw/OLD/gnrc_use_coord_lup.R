#' use_coord_lup
#' @description S4 Generic function to get the value of the slot use_coord_lup
#' @rdname use_coord_lup-methods
#' @param x An object 
#' 
#' @export

methods::setGeneric("use_coord_lup", function(x) standardGeneric("use_coord_lup"))
#' use_coord_lup
#' @name use_coord_lup-ready4_profiled_area
#' @description Get the value of the slot use_coord_lup for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname use_coord_lup-methods
#' @aliases use_coord_lup,ready4_profiled_area-method
methods::setMethod("use_coord_lup", methods::className("ready4_profiled_area"), function (x) 
{
    x@use_coord_lup
})
#' use_coord_lup<-
#' @description S4 Generic function to set the value of the slot use_coord_lup
#' @rdname use_coord_lup_set-methods
#' @param x An object 
#' @param value Value to be assigned to x
#' 
#' @export

methods::setGeneric("use_coord_lup<-", function(x, value) standardGeneric("use_coord_lup<-"))
#' use_coord_lup<-
#' @name use_coord_lup<--ready4_profiled_area
#' @description Set the value of the slot use_coord_lup for S4 objects of class ready4_profiled_area
#' @param x An object of class ready4_profiled_area
#' @rdname use_coord_lup_set-methods
#' @aliases use_coord_lup<-,ready4_profiled_area-method
methods::setMethod("use_coord_lup<-", methods::className("ready4_profiled_area"), function (x, value) 
{
    x@use_coord_lup <- value
    methods::validObject(x)
    x
})
