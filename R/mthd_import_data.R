#' Import data method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description import_data.ready4_sp_import_lup() is an Import Data method that imports data from saved files and loads them into memory as R objects. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param included_items_names PARAM_DESCRIPTION
#' @param item_data_type PARAM_DESCRIPTION
#' @param data_directory PARAM_DESCRIPTION
#' @param r_data_dir_chr R data directory (a character vector)
#' @param save_lgl Save (a logical vector), Default: T
#' @return NA ()
#' @rdname import_data-methods
#' @export 
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_chr map
#' @importFrom sf st_read
#' @importFrom stats setNames
import_data.ready4_sp_import_lup <- function (x, included_items_names, item_data_type, data_directory, 
    r_data_dir_chr, save_lgl = T) 
{
    downloaded_data_tb <- x %>% dplyr::filter(data_type == item_data_type) %>% 
        dplyr::mutate(inc_file_main = ifelse(is.null(x$new_names_for_inc_files[[1]]), 
            inc_file_main, ifelse(is.na(new_names_for_inc_files %>% 
                unlist()), inc_file_main, purrr::map_chr(new_names_for_inc_files, 
                ~.x[[1]]))))
    path_vec <- purrr::map_chr(included_items_names, ~get_sngl_path_for_imp(downloaded_data_tb = downloaded_data_tb %>% 
        dplyr::select(c(name, country, area_type, region, main_feature, 
            year, inc_file_main)), lookup_reference = .x, data_directory = data_directory))
    r_import_path_chr <- get_r_import_path_chr(r_data_dir_chr = r_data_dir_chr, 
        name_chr = x$name, data_type_chr = item_data_type)
    if (item_data_type == "Geometry") {
        item_list <- purrr::map(path_vec, ~{
            if (!save_lgl & file.exists(r_import_path_chr)) {
                "SKIP_IMPORT"
            }
            else {
                sf::st_read(dsn = .x, layer = get_name_from_path_chr(.x, 
                  with_ext = FALSE))
            }
        }) %>% stats::setNames(included_items_names)
    }
    else {
        item_list <- purrr::map(path_vec, ~{
            if (!save_lgl & file.exists(r_import_path_chr)) {
                "SKIP_IMPORT"
            }
            else {
                get_non_shape_items_for_imp(.x, x = downloaded_data_tb)
            }
        }) %>% stats::setNames(included_items_names)
    }
    return(item_list)
}
#' @rdname import_data-methods
#' @aliases import_data,ready4_sp_import_lup-method
methods::setMethod("import_data", "ready4_sp_import_lup", import_data.ready4_sp_import_lup)
