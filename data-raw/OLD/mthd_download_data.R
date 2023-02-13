#' Download data method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description authorData.vicinity_raw() is a Download Data method that downloads data files. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param dir_1L_chr PARAM_DESCRIPTION
#' @param data_match_value_xx PARAM_DESCRIPTION
#' @param match_var_nm_1L_chr PARAM_DESCRIPTION, Default: 'name'
#' @param sub_dirs_chr PARAM_DESCRIPTION, Default: NULL
#' @param overwrite_1L_lgl Overwrite (a logical vector), Default: F
#' @return NULL
#' @rdname authorData-methods
#' @export 

authorData.vicinity_raw <- function (x, dir_1L_chr, data_match_value_xx, match_var_nm_1L_chr = "name", 
    sub_dirs_chr = NULL, overwrite_1L_lgl = F) 
{
    if (is.null(sub_dirs_chr)) 
        sub_dirs_chr <- names(vicinity_raw())[names(vicinity_raw()) %in% 
            c("country", "area_type", "region", "main_feature", 
                "year")]
    directory_paths <- make_write_paths_chr(x = x, dir_1L_chr = dir_1L_chr, 
        data_match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
        sub_dirs_chr = sub_dirs_chr)
    write_dirs_for_imp(directory_paths = directory_paths)
    write_fls_for_imp(x = x, data_match_value_xx = data_match_value_xx, 
        match_var_nm_1L_chr = match_var_nm_1L_chr, directory_path = directory_paths[length(directory_paths)], 
        overwrite_1L_lgl = overwrite_1L_lgl)
}
#' @rdname authorData-methods
#' @aliases authorData,vicinity_raw-method
methods::setMethod("authorData", "vicinity_raw", authorData.vicinity_raw)
