#' Save raw method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description save_raw.vicinity_raw() is a Save Raw method that saves the native version of a file format. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param required_data PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param overwrite_1L_lgl Overwrite (a logical vector), Default: F
#' @return NULL
#' @rdname save_raw-methods
#' @export 
#' @importFrom purrr map_lgl
save_raw.vicinity_raw <- function (x, required_data, destination_directory, overwrite_1L_lgl = F) 
{
    purrr::map_lgl(required_data, ~download_data(x = x, destination_directory = destination_directory, 
        data_match_value_xx = .x, overwrite_1L_lgl = overwrite_1L_lgl))
}
#' @rdname save_raw-methods
#' @aliases save_raw,vicinity_raw-method
methods::setMethod("save_raw", "vicinity_raw", save_raw.vicinity_raw)
