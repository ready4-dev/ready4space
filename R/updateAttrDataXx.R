#' updateAttrDataXx
#' S4 Generic function to update attribute data object.
#' @name updateAttrDataXx
#' @param x An object
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1#'  }
#' }
#' @seealso
#'  \code{\link[methods]{setGeneric}}
#' @export
#' @importFrom methods setGeneric
methods::setGeneric("updateAttrDataXx",
                    function(x,...,verbose=TRUE) standardGeneric("updateAttrDataXx"),
                    signature = "x")
#'
#' Return input object unaltered if no method for child class defined.
#' @param x An object of class class ready4_lookup
#' @param attr_data_xx An attribute data object - could be a dataframe or a list.
#' @param alt_names_sf
#' @param area_names_var_str
#' @param region_short_long_vec
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[methods]{setMethod}},\code{\link[methods]{validObject}}
#' @export
#' @importFrom methods setMethod validObject
#' @import ready4s4
#' @describeIn updateAttrDataXx Method for S4 class ready4_lookup
methods::setMethod("updateAttrDataXx",
                   "ready4_lookup",
                   function(x,
                            attr_data_xx,
                            alt_names_sf,
                            area_names_var_str,
                            region_short_long_vec) {
                     attr_data_xx
                   })
