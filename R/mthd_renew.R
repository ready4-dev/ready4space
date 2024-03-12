#' Renew (update) values
#' @description renew.vicinity_points() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 submodule class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes. The function is called for its side effects and does not return a value.
#' @param x An instance of `vicinity_points`, a ready4 submodule class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param service_type_chr Service type (a character vector), Default: character(0)
#' @param cluster_name_chr Cluster name (a character vector), Default: character(0)
#' @param service_name_chr Service name (a character vector), Default: character(0)
#' @param lat_dbl Lat (a double vector), Default: numeric(0)
#' @param lng_dbl Longitude (a double vector), Default: numeric(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'table'
#' @param ... Additional arguments
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
renew.vicinity_points <- function (x, service_type_chr = character(0), cluster_name_chr = character(0), 
    service_name_chr = character(0), lat_dbl = numeric(0), lng_dbl = numeric(0), 
    what_1L_chr = "table", ...) 
{
    if (what_1L_chr == "table") {
        fn_env_ls <- as.list(rlang::current_env())[-1]
        x <- ready4::update_tb_r3(x, fn = renew.vicinity_points, 
            fn_env_ls = fn_env_ls)
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,vicinity_points-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("vicinity_points", package = "vicinity"), renew.vicinity_points)
#' Renew (update) values
#' @description renew.vicinity_processed() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data). The function is called for its side effects and does not return a value.
#' @param x An instance of `vicinity_processed`, a ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param args_ls Arguments (a list), Default: NULL
#' @param additional_detail_chr Additional detail (a character vector), Default: character(0)
#' @param area_type_chr Area type (a character vector), Default: character(0)
#' @param area_bndy_yr_chr Area boundary year (a character vector), Default: character(0)
#' @param country_chr Country (a character vector), Default: character(0)
#' @param data_type_chr Data type (a character vector), Default: character(0)
#' @param main_feature_chr Main feature (a character vector), Default: character(0)
#' @param name_chr Name (a character vector), Default: character(0)
#' @param region_chr Region (a character vector), Default: character(0)
#' @param source_reference_chr Source reference (a character vector), Default: character(0)
#' @param year_chr Year (a character vector), Default: character(0)
#' @param year_end_chr Year end (a character vector), Default: character(0)
#' @param year_start_chr Year start (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'table'
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
renew.vicinity_processed <- function (x, args_ls = NULL, additional_detail_chr = character(0), 
    area_type_chr = character(0), area_bndy_yr_chr = character(0), 
    country_chr = character(0), data_type_chr = character(0), 
    main_feature_chr = character(0), name_chr = character(0), 
    region_chr = character(0), source_reference_chr = character(0), 
    year_chr = character(0), year_end_chr = character(0), year_start_chr = character(0), 
    what_1L_chr = "table") 
{
    if (what_1L_chr == "names") {
        x <- add_names(x)
    }
    if (what_1L_chr == "table") {
        if (!is.null(args_ls)) {
            x <- renew.vicinity_processed(x, args_ls = NULL, 
                source_reference_chr = args_ls[[2]], area_type_chr = args_ls[[3]], 
                area_bndy_yr_chr = args_ls[[4]], region_chr = args_ls[[5]], 
                year_chr = args_ls[[6]], year_start_chr = args_ls[[7]], 
                year_end_chr = args_ls[[8]], main_feature_chr = args_ls[[9]])
        }
        else {
            fn_env_ls <- as.list(rlang::current_env())[-c(1, 
                2, 15)]
            x <- ready4::update_tb_r3(x, fn = renew.vicinity_processed, 
                fn_env_ls = fn_env_ls)
        }
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,vicinity_processed-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("vicinity_processed", package = "vicinity"), renew.vicinity_processed)
#' Renew (update) values
#' @description renew.vicinity_raw() is a renew method that renews an instance of a class by updating it with new data. This method is implemented for the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function is called for its side effects and does not return a value.
#' @param x An instance of `vicinity_raw`, a ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param area_type_chr Area type (a character vector), Default: character(0)
#' @param area_bndy_yr_chr Area boundary year (a character vector), Default: character(0)
#' @param country_chr Country (a character vector), Default: character(0)
#' @param data_type_chr Data type (a character vector), Default: character(0)
#' @param main_feature_chr Main feature (a character vector), Default: character(0)
#' @param name_chr Name (a character vector), Default: character(0)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one), Default: character(0)
#' @param region_chr Region (a character vector), Default: character(0)
#' @param source_reference_chr Source reference (a character vector), Default: character(0)
#' @param year_chr Year (a character vector), Default: character(0)
#' @param year_end_chr Year end (a character vector), Default: ycharacter(0)
#' @param year_start_chr Year start (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'table'
#' @return x (An object)
#' @rdname renew-methods
#' @export 
#' @importFrom dplyr filter select mutate pull bind_rows
#' @importFrom purrr map map2 reduce map_dbl pluck
#' @importFrom stringr str_locate str_sub
#' @importFrom rlang current_env
#' @importFrom ready4 update_tb_r3 renew
renew.vicinity_raw <- function (x, area_type_chr = character(0), area_bndy_yr_chr = character(0), 
    country_chr = character(0), data_type_chr = character(0), 
    main_feature_chr = character(0), name_chr = character(0), 
    processed_fls_dir_1L_chr = character(0), region_chr = character(0), 
    source_reference_chr = character(0), year_chr = character(0), 
    year_end_chr = ycharacter(0), year_start_chr = character(0), 
    what_1L_chr = "table") 
{
    if (what_1L_chr == "names") {
        x <- add_names(x)
    }
    if (what_1L_chr == "order") {
        not_to_be_ordered_tb <- x %>% dplyr::filter(is.na(uid_chr))
        x <- x %>% dplyr::filter(!is.na(uid_chr))
        ordering_tb <- x %>% dplyr::select(name_chr, uid_chr, 
            add_bndys_from_ls) %>% dplyr::mutate(preceeded_by = purrr::map(add_bndys_from_ls, 
            ~unlist(.x)[unlist(.x) %in% uid_chr])) %>% dplyr::mutate(sequence = purrr::map2(preceeded_by, 
            uid_chr, ~c(.x, .y)))
        if (nrow(x) > 0) {
            ordering_chr <- purrr::reduce(ordering_tb %>% dplyr::pull(sequence), 
                ~append(.x, .y[!.y %in% .x]))
            x <- x[match(ordering_chr, x$uid_chr), ]
        }
        dplyr::bind_rows(x, not_to_be_ordered_tb)
    }
    if (what_1L_chr == "shiny") {
        x <- x %>% dplyr::mutate(start_from = purrr::map_dbl(source_reference_chr, 
            ~2 + stringr::str_locate(.x, ":") %>% purrr::pluck(1))) %>% 
            dplyr::mutate(start_from = purrr::map_dbl(start_from, 
                ~ifelse(is.na(.x), 1, .x))) %>% dplyr::mutate(shiny_source_chr = paste0(processed_fls_dir_1L_chr, 
            "/", stringr::str_sub(source_reference_chr, start = start_from), 
            ".RDS"))
    }
    if (what_1L_chr == "table") {
        fn_env_ls <- as.list(rlang::current_env())[-1]
        x <- ready4::update_tb_r3(x, fn = renew.vicinity_raw, 
            fn_env_ls = fn_env_ls)
    }
    return(x)
}
#' @rdname renew-methods
#' @aliases renew,vicinity_raw-method
#' @importFrom ready4 renew
methods::setMethod("renew", methods::className("vicinity_raw", package = "vicinity"), renew.vicinity_raw)
#' 
#' Renew (update) values
#' @name renew-VicinityLookup
#' @description renew method applied to VicinityLookup
#' @param x An object of class VicinityLookup
#' @param package_1L_chr Package (a character vector of length one), Default: character(0)
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param tbl_data_type_1L_chr Table data type (a character vector of length one), Default: 'Geometry'
#' @param template_ls Template (a list), Default: NULL
#' @param what_1L_chr What (a character vector of length one), Default: 'processed'
#' @return x (An object of class VicinityLookup)
#' @rdname renew-methods
#' @aliases renew,VicinityLookup-method
#' @export 
#' @importFrom purrr map2 reduce map2_chr pmap_dfr
#' @importFrom ready4 get_from_lup_obj renew
#' @importFrom dplyr mutate filter select pull arrange
#' @importFrom stringr str_which
#' @importFrom tibble tibble add_row
methods::setMethod("renew", "VicinityLookup", function (x, package_1L_chr = character(0), path_1L_chr = character(0), 
    tbl_data_type_1L_chr = "Geometry", template_ls = NULL, what_1L_chr = "processed") 
{
    if (what_1L_chr == "processed") {
        data_pk_lup_arguments_ls <- purrr::map2(template_ls, 
            names(template_ls), ~list(.x, .y, ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                target_var_nm_1L_chr = "area_type_chr", match_var_nm_1L_chr = "name_chr", 
                match_value_xx = .y, evaluate_1L_lgl = FALSE), 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                  target_var_nm_1L_chr = "area_bndy_yr_chr", 
                  match_var_nm_1L_chr = "name_chr", match_value_xx = .y, 
                  evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                  target_var_nm_1L_chr = "region_chr", match_var_nm_1L_chr = "name_chr", 
                  match_value_xx = .y, evaluate_1L_lgl = FALSE), 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                  target_var_nm_1L_chr = "year_chr", match_var_nm_1L_chr = "name_chr", 
                  match_value_xx = .y, evaluate_1L_lgl = FALSE), 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                  target_var_nm_1L_chr = "year_start_chr", match_var_nm_1L_chr = "name_chr", 
                  match_value_xx = .y, evaluate_1L_lgl = FALSE), 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                  target_var_nm_1L_chr = "year_end_chr", match_var_nm_1L_chr = "name_chr", 
                  match_value_xx = .y, evaluate_1L_lgl = FALSE), 
                ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3, 
                  target_var_nm_1L_chr = "main_feature_chr", 
                  match_var_nm_1L_chr = "name_chr", match_value_xx = .y, 
                  evaluate_1L_lgl = FALSE)))
        y_vicinity_processed <- purrr::reduce(data_pk_lup_arguments_ls, 
            .init = x@vicinity_processed_r3, ~renew.vicinity_processed(.x, 
                .y)) %>% dplyr::mutate(data_type_chr = tbl_data_type_1L_chr)
        package_1L_chr <- ifelse(package_1L_chr == "" | is.na(package_1L_chr), 
            "", paste0(package_1L_chr, "::"))
        y_vicinity_processed <- y_vicinity_processed %>% dplyr::mutate(source_reference_chr = paste0(package_1L_chr, 
            source_reference_chr)) %>% dplyr::mutate(source_reference_chr = purrr::map2_chr(main_feature_chr, 
            source_reference_chr, ~ifelse(.x == "Boundary", paste0(.y, 
                "_sf"), .y)))
        x <- renewSlot(x, "vicinity_processed_r3", y_vicinity_processed)
    }
    if (what_1L_chr == "resolutions") {
        dr_dp_tb <- x@vicinity_processed_r3 %>% dplyr::filter(main_feature_chr == 
            "Boundary") %>% dplyr::select(area_type_chr, country_chr, 
            region_chr, source_reference_chr, year_chr) %>% dplyr::mutate(source_reference_chr = paste0(path_1L_chr, 
            "/", source_reference_chr, ".RDS"))
        dr_dp_vec <- dr_dp_tb %>% dplyr::pull(source_reference_chr)
        dr_nt_vec <- dr_dp_tb %>% dplyr::pull(region_chr)
        if (any(dr_nt_vec == "National")) {
            nat_sf <- readRDS(dr_dp_vec[stringr::str_which(dr_nt_vec, 
                "National") %>% min()])
            nat_area_1L_dbl <- nat_sf %>% make_km_sqd_dbl()
        }
        else {
            nat_area_1L_dbl <- NA_real_
        }
        resolution_lup_r3 <- purrr::pmap_dfr(dr_dp_tb, ~tibble::tibble(parent_area_chr = ..2, 
            boundary_year_dbl = as.numeric(..5), area_type_chr = ..1, 
            area_count_dbl = nrow(readRDS(..4)) %>% as.double(), 
            complete_lgl = T, summed_area_dbl = ifelse(..3 == 
                "National", nat_area_1L_dbl, readRDS(..4) %>% 
                make_km_sqd_dbl()), mean_size_dbl = summed_area_dbl/area_count_dbl))
        resolution_lup_r3 <- resolution_lup_r3 %>% vicinity_resolutions() %>% 
            dplyr::arrange(mean_size_dbl)
        x <- renewSlot(x, "vicinity_resolutions_r3", resolution_lup_r3)
    }
    if (what_1L_chr == "templates") {
        starter_sf_nm_1L_chr <- get_name_from_path_chr(path_1L_chr, 
            with_ext_1L_lgl = F)
        starter_sf_lup_r3 <- tibble::add_row(x@vicinity_templates_r3, 
            country_chr = x@vicinity_raw_r3 %>% dplyr::pull(country_chr), 
            area_type_chr = x@vicinity_raw_r3 %>% dplyr::pull(area_type_chr), 
            area_bndy_yr_chr = x@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr), 
            starter_sf_nm_chr = starter_sf_nm_1L_chr, subdivision_chr = x@vicinity_raw_r3 %>% 
                dplyr::pull(uid_chr))
        x <- renewSlot(x, "vicinity_templates_r3", starter_sf_lup_r3)
    }
    if (what_1L_chr == "identifiers") {
        uid_lup_r3 <- tibble::add_row(vicinity_identifiers(), 
            spatial_unit_chr = x@vicinity_raw_r3 %>% dplyr::pull(area_type_chr), 
            year_chr = x@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr), 
            var_name_chr = x@vicinity_raw_r3 %>% dplyr::pull(uid_chr))
        x <- renewSlot(x, "vicinity_identifiers_r3", uid_lup_r3)
    }
    return(x)
})
#' 
#' Renew (update) values
#' @name renew-VicinityArguments
#' @description renew method applied to VicinityArguments
#' @param x An object of class VicinityArguments
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @param write_1L_lgl Write (a logical vector of length one)
#' @return x (An object of class VicinityArguments)
#' @rdname renew-methods
#' @aliases renew,VicinityArguments-method
#' @export 
#' @importFrom ready4 renew
methods::setMethod("renew", "VicinityArguments", function (x, raw_fls_dir_1L_chr, write_1L_lgl) 
{
    x <- x %>% renewSlot("write_1L_lgl", write_1L_lgl) %>% renewSlot("raw_fls_dir_1L_chr", 
        raw_fls_dir_1L_chr)
    return(x)
})
