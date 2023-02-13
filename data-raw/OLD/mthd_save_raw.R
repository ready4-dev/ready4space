#' Save raw method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description author.vicinity_raw() is a Save Raw method that saves the native version of a file format. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param required_data PARAM_DESCRIPTION
#' @param dir_1L_chr PARAM_DESCRIPTION
#' @param overwrite_1L_lgl Overwrite (a logical vector), Default: F
#' @return NULL
#' @rdname author-methods
#' @export 
#' @importFrom purrr map_lgl
author.vicinity_raw <- function (x, required_data, dir_1L_chr, overwrite_1L_lgl = F) 
{
    purrr::map_lgl(required_data, ~authorData(x = x, dir_1L_chr = dir_1L_chr, 
        data_match_value_xx = .x, overwrite_1L_lgl = overwrite_1L_lgl))
}
#' @rdname author-methods
#' @aliases author,vicinity_raw-method
methods::setMethod("author", "vicinity_raw", author.vicinity_raw)
