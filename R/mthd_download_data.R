#' Download data method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description download_data.ready4_sp_import_lup() is a Download Data method that downloads data files. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION, Default: 'name'
#' @param directory_sub_divs PARAM_DESCRIPTION, Default: NULL
#' @param overwrite_lgl Overwrite (a logical vector), Default: F
#' @return NULL
#' @rdname download_data-methods
#' @export 

download_data.ready4_sp_import_lup <- function (x, destination_directory, data_lookup_ref, lookup_variable = "name", 
    directory_sub_divs = NULL, overwrite_lgl = F) 
{
    if (is.null(directory_sub_divs)) 
        directory_sub_divs <- names(ready4_sp_import_lup())[names(ready4_sp_import_lup()) %in% 
            c("country", "area_type", "region", "main_feature", 
                "year")]
    directory_paths <- get_dir_paths_for_data_imp(x = x, destination_directory = destination_directory, 
        data_lookup_ref = data_lookup_ref, lookup_variable = lookup_variable, 
        directory_sub_divs = directory_sub_divs)
    write_dirs_for_imp(directory_paths = directory_paths)
    write_fls_for_imp(x = x, data_lookup_ref = data_lookup_ref, 
        lookup_variable = lookup_variable, directory_path = directory_paths[length(directory_paths)], 
        overwrite_lgl = overwrite_lgl)
}
#' @rdname download_data-methods
#' @aliases download_data,ready4_sp_import_lup-method
methods::setMethod("download_data", "ready4_sp_import_lup", download_data.ready4_sp_import_lup)
