#' Author and document datasets
#' @description authorData.vicinity_raw() is an authorData method that authors and saves files necessary for creating and documenting datasets. This method is implemented for the ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function returns Files written (a logical vector of length one).
#' @param x An instance of ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param path_1L_chr Path (a character vector of length one)
#' @param data_match_value_xx Data match value (an output object of multiple potential types)
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: 'name'
#' @param sub_dirs_chr Sub directories (a character vector), Default: NULL
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @param what_1L_chr What (a character vector of length one), Default: 'outer'
#' @return Files written (a logical vector of length one)
#' @rdname authorData-methods
#' @export 
#' @importFrom purrr map_chr walk2
#' @importFrom ready4 get_from_lup_obj authorData
#' @importFrom dplyr select
#' @importFrom ready4use ready4use_dataverses
#' @importFrom utils download.file unzip
authorData.vicinity_raw <- function (x, path_1L_chr, data_match_value_xx, match_var_nm_1L_chr = "name", 
    sub_dirs_chr = NULL, overwrite_1L_lgl = F, what_1L_chr = "outer") 
{
    if (what_1L_chr == "outer") {
        if (is.null(sub_dirs_chr)) 
            sub_dirs_chr <- names(vicinity_raw())[names(vicinity_raw()) %in% 
                c("country_chr", "area_type_chr", "region_chr", 
                  "main_feature_chr", "year_chr")]
        paths_chr <- manufacture(x = x, processed_fls_dir_1L_chr = path_1L_chr, 
            match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
            sub_dirs_chr = sub_dirs_chr, what_1L_chr = "paths_chr")
        write_dirs_for_imp(paths_chr = paths_chr)
        files_written_1L_lgl <- authorData.vicinity_raw(x, data_match_value_xx = data_match_value_xx, 
            match_var_nm_1L_chr = match_var_nm_1L_chr, path_1L_chr = paths_chr[length(paths_chr)], 
            overwrite_1L_lgl = overwrite_1L_lgl, what_1L_chr = "inner")
    }
    if (what_1L_chr == "inner") {
        files_written_1L_lgl <- F
        download_components_chr <- purrr::map_chr(c("file_name_chr", 
            "file_type_chr", "download_url_chr", "inc_file_main_chr", 
            "local_file_src_chr", "data_repo_db_ui_chr"), ~ready4::get_from_lup_obj(data_lookup_tb = x, 
            match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
            target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
        dest_file_1L_chr <- paste0(path_1L_chr, "/", download_components_chr[1], 
            download_components_chr[2])
        if (!is.na(download_components_chr[5])) {
            if (overwrite_1L_lgl | file.exists(dest_file_1L_chr)) 
                file.copy(from = download_components_chr[5], 
                  to = dest_file_1L_chr)
        }
        else {
            if (!is.na(paste0(path_1L_chr, "/", download_components_chr[4]))) {
                if (overwrite_1L_lgl | !file.exists(paste0(path_1L_chr, 
                  "/", download_components_chr[4]))) {
                  if (!is.na(download_components_chr[6])) {
                    procure(x %>% dplyr::select(intersect(names(ready4use_dataverses()), 
                      names(vicinity_raw()))) %>% ready4use::ready4use_dataverses(), 
                      save_dir_path_chr = path_1L_chr, unlink_lgl = F)
                  }
                  else {
                    utils::download.file(download_components_chr[3], 
                      destfile = dest_file_1L_chr, mode = "wb")
                  }
                  if (download_components_chr[2] == ".zip") {
                    utils::unzip(dest_file_1L_chr, exdir = path_1L_chr)
                  }
                  capture_xx <- authorData.vicinity_raw(x, data_match_value_xx = data_match_value_xx, 
                    match_var_nm_1L_chr = match_var_nm_1L_chr, 
                    path_1L_chr = path_1L_chr, overwrite_1L_lgl = overwrite_1L_lgl, 
                    what_1L_chr == "rename")
                  files_written_1L_lgl <- T
                }
            }
        }
    }
    if (what_1L_chr == "rename") {
        files_written_1L_lgl <- NULL
        old_names_ls <- ready4::get_from_lup_obj(data_lookup_tb = x, 
            match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
            target_var_nm_1L_chr = "inc_fls_to_rename_ls", evaluate_1L_lgl = FALSE)
        new_names_ls <- ready4::get_from_lup_obj(data_lookup_tb = x, 
            match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
            target_var_nm_1L_chr = "new_nms_for_inc_fls_ls", 
            evaluate_1L_lgl = FALSE)
        if (!is.na(old_names_ls)) {
            purrr::walk2(old_names_ls, new_names_ls, ~if (overwrite_1L_lgl | 
                !file.exists(paste0(path_1L_chr, "/", .y))) {
                file.rename(paste0(path_1L_chr, "/", .x), paste0(path_1L_chr, 
                  "/", .y))
            })
        }
    }
    return(files_written_1L_lgl)
}
#' @rdname authorData-methods
#' @aliases authorData,vicinity_raw-method
#' @importFrom ready4 authorData
methods::setMethod("authorData", methods::className("vicinity_raw", package = "vicinity"), authorData.vicinity_raw)
#' 
#' Author and document datasets
#' @name authorData-VicinityLocal
#' @description authorData method applied to VicinityLocal
#' @param x An object of class VicinityLocal
#' @param return_r4_1L_lgl Return ready4 S4 (a logical vector of length one), Default: F
#' @return Return (an output object of multiple potential types)
#' @rdname authorData-methods
#' @aliases authorData,VicinityLocal-method
#' @export 
#' @importFrom ready4use assert_single_row_tb
#' @importFrom ready4 authorData
methods::setMethod("authorData", "VicinityLocal", function (x, return_r4_1L_lgl = F) 
{
    vicinity_raw_r3 <- x@a_VicinityLookup@vicinity_raw_r3
    ready4use::assert_single_row_tb(vicinity_raw_r3)
    raw_format_sp_dir <- write_raw_format_dir(data_type_chr = vicinity_raw_r3$data_type_chr, 
        raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr)
    imports_chr <- manufacture(x@a_VicinityLookup, type_1L_chr = vicinity_raw_r3$data_type_chr)
    write_1L_lgl <- author(x = vicinity_raw_r3, match_vals_xx = imports_chr, 
        path_1L_chr = raw_format_sp_dir, overwrite_1L_lgl = x@overwrite_1L_lgl)
    if (return_r4_1L_lgl) {
        return_xx <- metamorphose(x, imports_chr = imports_chr, 
            raw_fls_dir_1L_chr = raw_format_sp_dir, write_1L_lgl = write_1L_lgl)
    }
    return(return_xx)
})
