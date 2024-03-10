#' Ratify that input or output data meet validity criteria
#' @description ratify.vicinity_processed() is a ratify method that ratifies that an instance of a class conforms to specified criteria. This method is implemented for the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data). The function returns Ratified (a logical vector of length one).
#' @param x An instance of `vicinity_processed`, a ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param data_items_chr Data items (a character vector), Default: character(0)
#' @param key_var_1L_chr Key variable (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'population'
#' @return Ratified (a logical vector of length one)
#' @rdname ratify-methods
#' @export 
#' @importFrom ready4 ratify
ratify.vicinity_processed <- function (x, data_items_chr = character(0), key_var_1L_chr = character(0), 
    what_1L_chr = "population") 
{
    if (what_1L_chr == "population") {
        ratified_1L_lgl <- validate_popl_predns_incld(data_items_chr = data_items_chr, 
            data_lookup_tb = x, key_var_1L_chr = key_var_1L_chr)
    }
    return(ratified_1L_lgl)
}
#' @rdname ratify-methods
#' @aliases ratify,vicinity_processed-method
#' @importFrom ready4 ratify
methods::setMethod("ratify", methods::className("vicinity_processed", package = "vicinity"), ratify.vicinity_processed)
