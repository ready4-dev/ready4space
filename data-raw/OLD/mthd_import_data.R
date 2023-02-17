#' Import data method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description import_data.vicinity_raw() is an Import Data method that imports data from saved files and loads them into memory as R objects. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param imports_chr PARAM_DESCRIPTION
#' @param data_type_1L_chr PARAM_DESCRIPTION
#' @param raw_fls_dir_1L_chr PARAM_DESCRIPTION
#' @param processed_fls_dir_1L_chr R data directory (a character vector)
#' @param write_1L_lgl Save (a logical vector), Default: T
#' @return NA ()
#' @rdname import_data-methods
#' @export 
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_chr map
#' @importFrom sf st_read
#' @importFrom stats setNames
import_data.vicinity_raw <- function (x, imports_chr, data_type_1L_chr, raw_fls_dir_1L_chr, 
    processed_fls_dir_1L_chr, write_1L_lgl = T) 
{
    downloaded_data_tb <- x %>% dplyr::filter(data_type == data_type_1L_chr) %>% 
        dplyr::mutate(inc_file_main_chr = ifelse(is.null(x$new_nms_for_inc_fls_ls[[1]]), 
            inc_file_main_chr, ifelse(is.na(new_nms_for_inc_fls_ls %>% 
                unlist()), inc_file_main_chr, purrr::map_chr(new_nms_for_inc_fls_ls, 
                ~.x[[1]]))))
    path_vec <- purrr::map_chr(imports_chr, ~get_sngl_path_for_imp(downloaded_data_tb = downloaded_data_tb %>% 
        dplyr::select(c(name, country, area_type, region, main_feature, 
            year, inc_file_main_chr)), match_value_xx = .x, raw_fls_dir_1L_chr = raw_fls_dir_1L_chr))
    r_import_path_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
        name_chr = x$name, data_type_chr = data_type_1L_chr)
    if (data_type_1L_chr == "Geometry") {
        item_list <- purrr::map(path_vec, ~{
            if (!write_1L_lgl & file.exists(r_import_path_chr)) {
                "SKIP_IMPORT"
            }
            else {
                sf::st_read(dsn = .x, layer = get_name_from_path_chr(.x, 
                  with_ext_1L_lgl = FALSE))
            }
        }) %>% stats::setNames(imports_chr)
    }
    else {
        item_list <- purrr::map(path_vec, ~{
            if (!write_1L_lgl & file.exists(r_import_path_chr)) {
                "SKIP_IMPORT"
            }
            else {
                get_non_shape_items_for_imp(.x, x = downloaded_data_tb)
            }
        }) %>% stats::setNames(imports_chr)
    }
    return(item_list)
}
#' @rdname import_data-methods
#' @aliases import_data,vicinity_raw-method
methods::setMethod("import_data", "vicinity_raw", import_data.vicinity_raw)
