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
#' updateAttrDataXx
#' @description Return input object unaltered if no method for child class defined.
#' @name updateAttrDataXx-ready4_lookup
#' @param attr_data_xx An object
#' @param alt_names_sf A simple features object
#' @param area_names_var_str A character string
#' @param region_short_long_vec A character vector
#' @export
#' @rdname updateAttrDataXx
#' @include s4_ready4_lookup.R
methods::setMethod("updateAttrDataXx",
                   "ready4_lookup",
                   function(x,
                            attr_data_xx,
                            alt_names_sf,
                            area_names_var_str,
                            region_short_long_vec) {
                     attr_data_xx
                   })
