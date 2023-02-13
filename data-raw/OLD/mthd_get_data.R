#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table for spatial data abbreviations..
#' @description procure.ready4_sp_abbreviations_lup() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table for spatial data abbreviations.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table for spatial data abbreviations.
#' @param col_nm_1L_chr Column (a character vector), Default: 'short_name'
#' @param match_value_xx Value (a character vector)
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom ready4fun get_from_lup
procure.ready4_sp_abbreviations_lup <- function (x, col_nm_1L_chr = "short_name", match_value_xx) 
{
    ready4::get_from_lup_obj(data_lookup_tb = x, match_value_xx = match_value_xx, 
        match_var_nm_1L_chr = col_nm_1L_chr, target_var_nm_1L_chr = ifelse(col_nm_1L_chr == 
            "short_name", "long_name", "short_name"), evaluate_1L_lgl = FALSE)
}
#' @rdname get_data-methods
#' @aliases get_data,ready4_sp_abbreviations_lup-method
methods::setMethod("get_data", "ready4_sp_abbreviations_lup", procure.ready4_sp_abbreviations_lup)
#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data)..
#' @description procure.ready4_sp_data_pack_lup() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param col_nm_1L_chr Column (a character vector), Default: 'main_feature'
#' @param match_value_xx Value (a character vector)
#' @param processed_fls_dir_1L_chr R data directory (a character vector), Default: 'NA'
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom ready4fun get_from_lup
procure.ready4_sp_data_pack_lup <- function (x, col_nm_1L_chr = "main_feature", match_value_xx, processed_fls_dir_1L_chr = NA_character_) 
{
    if (!is.na(processed_fls_dir_1L_chr)) {
        x <- add_path_col(x, processed_fls_dir_1L_chr = processed_fls_dir_1L_chr)
    }
    readRDS(ready4::get_from_lup_obj(data_lookup_tb = x, match_value_xx = match_value_xx, 
        match_var_nm_1L_chr = col_nm_1L_chr, target_var_nm_1L_chr = "shiny_source_chr", 
        evaluate_1L_lgl = FALSE))
}
#' @rdname get_data-methods
#' @aliases get_data,ready4_sp_data_pack_lup-method
methods::setMethod("get_data", "ready4_sp_data_pack_lup", procure.ready4_sp_data_pack_lup)
#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects..
#' @description procure.vicinity_identifiers() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param col_nm_1L_chr Column (a character vector), Default: 'spatial_unit'
#' @param match_value_xx Value (a character vector)
#' @param area_bound_yr PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom ready4fun get_from_lup
#' @importFrom dplyr filter
procure.vicinity_identifiers <- function (x, col_nm_1L_chr = "spatial_unit", match_value_xx, area_bound_yr) 
{
    ready4::get_from_lup_obj(data_lookup_tb = x %>% dplyr::filter(year == 
        area_bound_yr), match_value_xx = match_value_xx, match_var_nm_1L_chr = col_nm_1L_chr, 
        target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "spatial_unit", "var_name", 
            "spatial_unit"), evaluate_1L_lgl = FALSE)
}
#' @rdname get_data-methods
#' @aliases get_data,vicinity_identifiers-method
methods::setMethod("get_data", "vicinity_identifiers", procure.vicinity_identifiers)
