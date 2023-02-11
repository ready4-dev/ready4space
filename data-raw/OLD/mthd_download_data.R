#' Download data method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description download_data.vicinity_raw() is a Download Data method that downloads data files. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_match_value_xx PARAM_DESCRIPTION
#' @param match_var_nm_1L_chr PARAM_DESCRIPTION, Default: 'name'
#' @param directory_sub_divs PARAM_DESCRIPTION, Default: NULL
#' @param overwrite_1L_lgl Overwrite (a logical vector), Default: F
#' @return NULL
#' @rdname download_data-methods
#' @export 

download_data.vicinity_raw <- function (x, destination_directory, data_match_value_xx, match_var_nm_1L_chr = "name", 
    directory_sub_divs = NULL, overwrite_1L_lgl = F) 
{
    if (is.null(directory_sub_divs)) 
        directory_sub_divs <- names(vicinity_raw())[names(vicinity_raw()) %in% 
            c("country", "area_type", "region", "main_feature", 
                "year")]
    directory_paths <- get_dir_paths_for_data_imp(x = x, destination_directory = destination_directory, 
        data_match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
        directory_sub_divs = directory_sub_divs)
    write_dirs_for_imp(directory_paths = directory_paths)
    write_fls_for_imp(x = x, data_match_value_xx = data_match_value_xx, 
        match_var_nm_1L_chr = match_var_nm_1L_chr, directory_path = directory_paths[length(directory_paths)], 
        overwrite_1L_lgl = overwrite_1L_lgl)
}
#' @rdname download_data-methods
#' @aliases download_data,vicinity_raw-method
methods::setMethod("download_data", "vicinity_raw", download_data.vicinity_raw)
