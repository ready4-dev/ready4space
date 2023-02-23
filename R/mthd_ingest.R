#' Ingest data
#' @description ingest.vicinity_processed() is an ingest method that ingests data saved in external files into R objects stored in working memory. This method is implemented for the ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data). The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param col_nm_1L_chr Column name (a character vector of length one), Default: 'main_feature_chr'
#' @param match_value_xx Match value (an output object of multiple potential types)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one), Default: 'NA'
#' @return Object (an output object of multiple potential types)
#' @rdname ingest-methods
#' @export 
#' @importFrom ready4 get_from_lup_obj ingest
ingest.vicinity_processed <- function (x, col_nm_1L_chr = "main_feature_chr", match_value_xx, 
    processed_fls_dir_1L_chr = NA_character_) 
{
    if (!is.na(processed_fls_dir_1L_chr)) {
        x <- renew(x, processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
            what_1L_chr = "shiny")
    }
    object_xx <- readRDS(ready4::get_from_lup_obj(data_lookup_tb = x, 
        match_value_xx = match_value_xx, match_var_nm_1L_chr = col_nm_1L_chr, 
        target_var_nm_1L_chr = "shiny_source_chr", evaluate_1L_lgl = FALSE))
    return(object_xx)
}
#' @rdname ingest-methods
#' @aliases ingest,vicinity_processed-method
#' @importFrom ready4 ingest
methods::setMethod("ingest", methods::className("vicinity_processed", package = "vicinity"), ingest.vicinity_processed)
#' Ingest data
#' @description ingest.vicinity_raw() is an ingest method that ingests data saved in external files into R objects stored in working memory. This method is implemented for the ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function returns Ingest (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param imports_chr Imports (a character vector), Default: character(0)
#' @param data_type_1L_chr Data type (a character vector of length one), Default: character(0)
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one), Default: character(0)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'list'
#' @param write_1L_lgl Write (a logical vector of length one), Default: T
#' @return Ingest (an output object of multiple potential types)
#' @rdname ingest-methods
#' @export 
#' @importFrom dplyr filter mutate
#' @importFrom purrr map_chr map
#' @importFrom sf st_read
#' @importFrom stats setNames
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
#' @importFrom ready4 get_from_lup_obj ingest
ingest.vicinity_raw <- function (x, imports_chr = character(0), data_type_1L_chr = character(0), 
    path_1L_chr = character(0), raw_fls_dir_1L_chr = character(0), 
    processed_fls_dir_1L_chr = character(0), what_1L_chr = "list", 
    write_1L_lgl = T) 
{
    if (what_1L_chr == "list") {
        downloaded_data_tb <- x %>% dplyr::filter(data_type_chr == 
            data_type_1L_chr) %>% dplyr::mutate(inc_file_main_chr = ifelse(is.null(x$new_nms_for_inc_fls_ls[[1]]), 
            inc_file_main_chr, ifelse(is.na(new_nms_for_inc_fls_ls %>% 
                unlist()), inc_file_main_chr, purrr::map_chr(new_nms_for_inc_fls_ls, 
                ~.x[[1]]))))
        path_vec <- purrr::map_chr(imports_chr, ~manufacture.vicinity_raw(downloaded_data_tb, 
            match_value_xx = .x, raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, 
            what_1L_chr = "path"))
        r_import_path_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
            name_chr = x$name, data_type_chr = data_type_1L_chr)
        if (data_type_1L_chr == "Geometry") {
            ingest_ls <- purrr::map(path_vec, ~{
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
            ingest_ls <- purrr::map(path_vec, ~{
                if (!write_1L_lgl & file.exists(r_import_path_chr)) {
                  "SKIP_IMPORT"
                }
                else {
                  ingest.vicinity_raw(x = downloaded_data_tb, 
                    path_1L_chr = .x, what_1L_chr = "non-shape")
                }
            }) %>% stats::setNames(imports_chr)
        }
        ingest_xx <- ingest_ls
    }
    if (what_1L_chr == "non-shape") {
        file_name <- get_name_from_path_chr(path_1L_chr)
        file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, 
            "\\.")[, 2] %>% as.vector())
        data_type_chr <- ready4::get_from_lup_obj(data_lookup_tb = x, 
            match_value_xx = file_name, match_var_nm_1L_chr = "inc_file_main_chr", 
            target_var_nm_1L_chr = "data_type_chr", evaluate_1L_lgl = FALSE)
        var_name_vec <- c("area_type_chr", "main_feature_chr", 
            "year_chr", "region")
        var_val_chr <- purrr::map_chr(var_name_vec, ~ready4::get_from_lup_obj(data_lookup_tb = procure.vicinity_raw(x, 
            match_value_xx = data_type_chr, what_1L_chr == "match"), 
            match_value_xx = file_name, match_var_nm_1L_chr = "inc_file_main_chr", 
            target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
        ingest_xx <- manufacture.vicinity_raw(x, var_val_chr = var_val_chr, 
            path_1L_chr = path_1L_chr)
    }
    return(ingest_xx)
}
#' @rdname ingest-methods
#' @aliases ingest,vicinity_raw-method
#' @importFrom ready4 ingest
methods::setMethod("ingest", methods::className("vicinity_raw", package = "vicinity"), ingest.vicinity_raw)
#' 
#' Ingest data
#' @name ingest-VicinityProfile
#' @description ingest method applied to VicinityProfile
#' @param x An object of class VicinityProfile
#' @param key_var_1L_chr Key variable (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'starter'
#' @return Object (an output object of multiple potential types)
#' @rdname ingest-methods
#' @aliases ingest,VicinityProfile-method
#' @export 
#' @importFrom dplyr filter
#' @importFrom ready4 get_from_lup_obj ingest
#' @importFrom stringr str_sub
#' @importFrom sf `st_crs<-`
#' @importFrom rlang sym
methods::setMethod("ingest", "VicinityProfile", function (x, key_var_1L_chr = character(0), what_1L_chr = "starter") 
{
    if (what_1L_chr == "starter") {
        sp_data_starter_sf_lup <- x@a_VicinityLookup@vicinity_templates_r3 %>% 
            dplyr::filter(country_chr == x@country_chr)
        if (!is.na(x@area_bndy_yr_dbl)) 
            sp_data_starter_sf_lup <- sp_data_starter_sf_lup %>% 
                dplyr::filter(area_bndy_yr_chr == x@area_bndy_yr_chr)
        starter_sf_nm_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = sp_data_starter_sf_lup, 
            match_var_nm_1L_chr = "area_type_chr", match_value_xx = ifelse(x@area_type_chr %in% 
                sp_data_starter_sf_lup$area_type_chr, x@area_type_chr, 
                x@region_type_chr), target_var_nm_1L_chr = "starter_sf", 
            evaluate_1L_lgl = FALSE)
        starter_sf <- ingest.vicinity_processed(x@a_VicinityLookup@vicinity_processed_r3, 
            col_nm_1L_chr = "name_chr", match_value_xx = starter_sf_nm_1L_chr %>% 
                stringr::str_sub(end = -4))
        if (x@use_coord_lup_lgl) {
            starter_sf <- starter_sf %>% sf::`st_crs<-`(x@crs_dbl[1])
        }
        else {
            starter_sf <- starter_sf %>% dplyr::filter(!!rlang::sym(key_var_1L_chr) %in% 
                x@features_chr)
        }
        object_xx <- starter_sf
    }
    return(object_xx)
})
