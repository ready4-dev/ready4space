#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table for spatial data abbreviations..
#' @description get_data.ready4_sp_abbreviations_lup() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table for spatial data abbreviations.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table for spatial data abbreviations.
#' @param col_chr Column (a character vector), Default: 'short_name'
#' @param value_chr Value (a character vector)
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom ready4fun get_from_lup
get_data.ready4_sp_abbreviations_lup <- function (x, col_chr = "short_name", value_chr) 
{
    ready4fun::get_from_lup(data_lookup_tb = x, lookup_reference = value_chr, 
        lookup_variable = col_chr, target_variable = ifelse(col_chr == 
            "short_name", "long_name", "short_name"), evaluate = FALSE)
}
#' @rdname get_data-methods
#' @aliases get_data,ready4_sp_abbreviations_lup-method
methods::setMethod("get_data", "ready4_sp_abbreviations_lup", get_data.ready4_sp_abbreviations_lup)
#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data)..
#' @description get_data.ready4_sp_data_pack_lup() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param col_chr Column (a character vector), Default: 'main_feature'
#' @param value_chr Value (a character vector)
#' @param r_data_dir_chr R data directory (a character vector), Default: 'NA'
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom ready4fun get_from_lup
get_data.ready4_sp_data_pack_lup <- function (x, col_chr = "main_feature", value_chr, r_data_dir_chr = NA_character_) 
{
    if (!is.na(r_data_dir_chr)) {
        x <- add_path_col(x, r_data_dir_chr = r_data_dir_chr)
    }
    readRDS(ready4fun::get_from_lup(data_lookup_tb = x, lookup_reference = value_chr, 
        lookup_variable = col_chr, target_variable = "shiny_source", 
        evaluate = FALSE))
}
#' @rdname get_data-methods
#' @aliases get_data,ready4_sp_data_pack_lup-method
methods::setMethod("get_data", "ready4_sp_data_pack_lup", get_data.ready4_sp_data_pack_lup)
#' Get data method applied toeadyforwhatsnext S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects..
#' @description get_data.ready4_sp_uid_lup() is a Get Data method that retrieves data from R objects loaded in memory. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of unique feature identifiers used for different spatial objects.
#' @param col_chr Column (a character vector), Default: 'spatial_unit'
#' @param value_chr Value (a character vector)
#' @param area_bound_yr PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_data-methods
#' @export 
#' @importFrom ready4fun get_from_lup
#' @importFrom dplyr filter
get_data.ready4_sp_uid_lup <- function (x, col_chr = "spatial_unit", value_chr, area_bound_yr) 
{
    ready4fun::get_from_lup(data_lookup_tb = x %>% dplyr::filter(year == 
        area_bound_yr), lookup_reference = value_chr, lookup_variable = col_chr, 
        target_variable = ifelse(col_chr == "spatial_unit", "var_name", 
            "spatial_unit"), evaluate = FALSE)
}
#' @rdname get_data-methods
#' @aliases get_data,ready4_sp_uid_lup-method
methods::setMethod("get_data", "ready4_sp_uid_lup", get_data.ready4_sp_uid_lup)
