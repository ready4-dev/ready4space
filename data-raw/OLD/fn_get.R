#' Get area sqkm
#' @description get_area_sqkm_sf() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get area sqkm simple features object. Function argument data_sf specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param data_sf Data (a simple features object)
#' @return NULL
#' @rdname get_area_sqkm_sf
#' @export 
#' @importFrom dplyr mutate summarise pull
#' @importFrom sf st_area
#' @importFrom units set_units
get_area_sqkm_sf <- function (data_sf) 
{
    data_sf %>% dplyr::mutate(FT_AREA_SQKM = sf::st_area(.) %>% 
        units::set_units(km^2)) %>% dplyr::summarise(TOT_AREA_SQKM = sum(FT_AREA_SQKM)) %>% 
        dplyr::pull(TOT_AREA_SQKM)
}
#' Get closest year
#' @description get_closest_year() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get closest year. Function argument data_lookup_tb specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param data_lookup_tb Data lookup (a tibble)
#' @param inc_main_ft_vec PARAM_DESCRIPTION
#' @param target_year PARAM_DESCRIPTION
#' @param target_area PARAM_DESCRIPTION, Default: NULL
#' @param find_closest PARAM_DESCRIPTION, Default: 'abs'
#' @return NA ()
#' @rdname get_closest_year
#' @export 
#' @importFrom dplyr filter pull
#' @importFrom purrr map
get_closest_year <- function (data_lookup_tb, inc_main_ft_vec, target_year, target_area = NULL, 
    find_closest = "abs") 
{
    if (!is.null(target_area)) {
        data_lookup_tb <- data_lookup_tb %>% dplyr::filter(area_type == 
            target_area)
    }
    avail_years <- purrr::map(inc_main_ft_vec, ~data_lookup_tb %>% 
        dplyr::filter(main_feature == .x) %>% dplyr::pull(year) %>% 
        as.numeric())
    if (find_closest == "abs") {
        closest_year <- purrr::map(avail_years, ~.x[which(abs(.x - 
            as.numeric(target_year)) == min(abs(.x - as.numeric(target_year))))])
    }
    if (find_closest == "previous") {
        closest_year <- purrr::map(avail_years, ~.x[which(as.numeric(target_year) - 
            .x == min(max(as.numeric(target_year) - .x, 0)))])
    }
    if (find_closest == "next") {
        closest_year <- purrr::map(avail_years, ~.x[which(.x - 
            as.numeric(target_year) == min(max(.x - as.numeric(target_year), 
            0)))])
    }
    return(closest_year)
}
#' Get common vars
#' @description get_common_vars_sf_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get common vars simple features object list. Function argument sf_ls specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return NULL
#' @rdname get_common_vars_sf_ls
#' @export 
#' @importFrom purrr map
get_common_vars_sf_ls <- function (sf_ls) 
{
    vec_ls <- purrr::map(sf_ls, ~names(.x))
    Reduce(intersect, vec_ls)
}
#' Get common yrs
#' @description get_common_yrs_sf_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get common yrs simple features object list. Function argument sf_ls specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return NULL
#' @rdname get_common_yrs_sf_ls
#' @export 
#' @importFrom purrr map
get_common_yrs_sf_ls <- function (sf_ls) 
{
    vec_ls <- purrr::map(list_of_sfs, ~get_included_yrs_sf(.x))
    Reduce(intersect, vec_ls)
}
#' Get data year
#' @description get_data_year_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get data year character vector. Function argument data_ymdhms specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param data_ymdhms PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_data_year_chr
#' @export 
#' @importFrom lubridate year
get_data_year_chr <- function (data_ymdhms) 
{
    data_ymdhms %>% lubridate::year() %>% as.character()
}
#' Get directory paths for data import
#' @description get_dir_paths_for_data_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get directory paths for data import. Function argument x specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_match_value_xx PARAM_DESCRIPTION
#' @param match_var_nm_1L_chr PARAM_DESCRIPTION
#' @param directory_sub_divs PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_dir_paths_for_data_imp
#' @export 
#' @importFrom purrr map_chr accumulate
#' @importFrom ready4fun get_from_lup
get_dir_paths_for_data_imp <- function (x, destination_directory, data_match_value_xx, match_var_nm_1L_chr, 
    directory_sub_divs) 
{
    directory_names <- purrr::map_chr(directory_sub_divs, ~ready4::get_from_lup_obj(data_lookup_tb = x, 
        match_value_xx = data_match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
        target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
    purrr::accumulate(directory_names, ~paste0(.x, "/", .y)) %>% 
        paste0(destination_directory, "/", .)
}
#' Get group by var
#' @description get_group_by_var() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get group by var. Function argument profile_unit specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param profile_unit PARAM_DESCRIPTION
#' @param data_unit PARAM_DESCRIPTION
#' @param group_at_profile_unit PARAM_DESCRIPTION, Default: TRUE
#' @param group_by_lookup_tb Group by lookup (a tibble)
#' @param area_bound_year PARAM_DESCRIPTION
#' @return NA ()
#' @rdname get_group_by_var
#' @export 
#' @importFrom ready4fun get_from_lup
#' @importFrom dplyr filter
get_group_by_var <- function (profile_unit, data_unit, group_at_profile_unit = TRUE, 
    group_by_lookup_tb, area_bound_year) 
{
    group_by <- ifelse(group_at_profile_unit, ready4::get_from_lup_obj(data_lookup_tb = group_by_lookup_tb %>% 
        dplyr::filter(spatial_unit == profile_unit) %>% dplyr::filter(as.numeric(year) == 
        area_bound_year), match_var_nm_1L_chr = "spatial_unit", match_value_xx = profile_unit, 
        target_var_nm_1L_chr = "var_name", evaluate_1L_lgl = FALSE), ready4::get_from_lup_obj(data_lookup_tb = group_by_lookup_tb, 
        match_var_nm_1L_chr = "spatial_unit", match_value_xx = data_unit, 
        target_var_nm_1L_chr = "var_name", evaluate_1L_lgl = FALSE))
    return(group_by)
}
#' Get group by var from pai
#' @description get_group_by_var_from_pai() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get group by var from pai. Function argument pa_r4 specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param pa_r4 Pa (a ready4 S4)
#' @return NA ()
#' @rdname get_group_by_var_from_pai
#' @export 

get_group_by_var_from_pai <- function (pa_r4) 
{
    group_by_lookup_tb = sp_uid_lup(pa_r4 %>% lookup_tb())
    if (!use_coord_lup(pa_r4)) {
        group_by_var_1L_chr <- get_group_by_var(profile_unit = pa_r4 %>% 
            area_type(), group_by_lookup_tb = group_by_lookup_tb, 
            area_bound_year = area_bound_year(pa_r4))
    }
    else {
        if (is.na(geom_dist_limit_km(pa_r4))) 
            group_by_var_1L_chr <- "drive_times"
        else group_by_var_1L_chr <- "distance_km"
        get_group_by_var(profile_unit = "GEOMETRIC_DISTANCE", 
            group_by_lookup_tb = group_by_lookup_tb)
    }
    return(group_by_var_1L_chr)
}
#' Get highest res
#' @description get_highest_res() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get highest res. Function argument options_vec specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param options_vec PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param resolution_lup_r3 Resolution (a ready4 S3 extension of lookup table)
#' @return NULL
#' @rdname get_highest_res
#' @export 

get_highest_res <- function (options_vec, year, resolution_lup_r3) 
{
    if (!is.na(options_vec[1])) {
        res_hierarchy <- get_resolution_hierarchy(data_year = as.numeric(year), 
            resolution_lup_r3 = resolution_lup_r3)
        res_hierarchy[min(which(res_hierarchy %in% options_vec))]
    }
    else NA
}
#' Get import character vector vec
#' @description get_imports_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get import character vector vec. Function argument lookup_tbs_r4 specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param lookup_tbs_r4 Lookup tibbles (a ready4 S4)
#' @param data_type_chr Data type (a character vector)
#' @return NULL
#' @rdname get_imports_chr
#' @export 
#' @importFrom dplyr filter pull
get_imports_chr <- function (lookup_tbs_r4, data_type_chr) 
{
    if (data_type_chr == "Geometry") {
        sp_import_lup(lookup_tbs_r4) %>% dplyr::filter(main_feature == 
            "Boundary") %>% dplyr::pull(name)
    }
    else {
        sp_import_lup(lookup_tbs_r4) %>% dplyr::filter(data_type == 
            "Attribute") %>% dplyr::pull(name)
    }
}
#' Get included yrs
#' @description get_included_yrs_sf() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get included yrs simple features object. Function argument sf specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param sf Simple features object (a simple features object)
#' @return NULL
#' @rdname get_included_yrs_sf
#' @export 
#' @importFrom sf `st_geometry<-`
#' @importFrom dplyr select starts_with
#' @importFrom stringr str_sub
get_included_yrs_sf <- function (sf) 
{
    sf %>% sf::`st_geometry<-`(NULL) %>% dplyr::select(dplyr::starts_with("y2")) %>% 
        names() %>% stringr::str_sub(start = 2, end = 5) %>% 
        unique() %>% as.numeric()
}
#' Get max or min yr of
#' @description get_max_or_min_yr_of_sf() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get max or min yr of simple features object. Function argument sf specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param sf Simple features object (a simple features object)
#' @param ... Additional arguments, Default: T
#' @return NULL
#' @rdname get_max_or_min_yr_of_sf
#' @export 

get_max_or_min_yr_of_sf <- function (sf, max = T) 
{
    year_vec <- get_included_yrs_sf(sf)
    if (max) 
        max(year_vec)
    else min(year_vec)
}
#' Get menu detail for import
#' @description get_menu_detail_for_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get menu detail for import. Function argument x specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_menu_detail_for_imp
#' @export 
#' @importFrom dplyr select
get_menu_detail_for_imp <- function (x) 
{
    x %>% dplyr::select(c(1:8, 12))
}
#' Get menu names for import
#' @description get_menu_names_for_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get menu names for import. Function argument x specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_menu_names_for_imp
#' @export 
#' @importFrom dplyr select pull
get_menu_names_for_imp <- function (x) 
{
    x %>% dplyr::select(name) %>% dplyr::pull()
}
#' Get menu of type detail for import
#' @description get_menu_of_type_detail_for_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get menu of type detail for import. Function argument x specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param match_value_xx PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_menu_of_type_detail_for_imp
#' @export 
#' @importFrom dplyr filter
get_menu_of_type_detail_for_imp <- function (x, match_value_xx) 
{
    x %>% dplyr::filter(data_type == match_value_xx)
}
#' Get menu of type names for import
#' @description get_menu_of_type_nms_for_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get menu of type names for import. Function argument x specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param match_value_xx PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_menu_of_type_nms_for_imp
#' @export 
#' @importFrom dplyr select pull
get_menu_of_type_nms_for_imp <- function (x, match_value_xx) 
{
    get_menu_of_type_detail_for_imp(x = x, match_value_xx = match_value_xx) %>% 
        dplyr::select(name) %>% dplyr::pull()
}
#' Get merge simple features object setter
#' @description get_merge_sf_str() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get merge simple features object setter. Function argument lookup_r4 specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param lookup_r4 Lookup (a ready4 S4)
#' @param sp_import_r3_slice PARAM_DESCRIPTION
#' @param processed_fls_dir_1L_chr PARAM_DESCRIPTION, Default: NULL
#' @return NULL
#' @rdname get_merge_sf_str
#' @export 
#' @importFrom dplyr pull
#' @importFrom purrr pluck map_chr
#' @importFrom ready4fun get_from_lup
#' @importFrom stringr str_detect
get_merge_sf_str <- function (lookup_r4, sp_import_r3_slice, processed_fls_dir_1L_chr = NULL) 
{
    if (is.null(sp_import_r3_slice %>% dplyr::pull(add_boundaries) %>% 
        purrr::pluck(1))) {
        NA_character_
    }
    else {
        if (is.na(sp_import_r3_slice %>% dplyr::pull(add_boundaries) %>% 
            purrr::pluck(1)) %>% any()) {
            NA_character_
        }
        else {
            purrr::map_chr(sp_import_r3_slice %>% pull(add_boundaries) %>% 
                purrr::pluck(1), ~ready4::get_from_lup_obj(data_lookup_tb = sp_import_lup(lookup_r4), 
                match_value_xx = .x, match_var_nm_1L_chr = "uid", 
                target_var_nm_1L_chr = "name", evaluate_1L_lgl = FALSE) %>% 
                ready4::get_from_lup_obj(data_lookup_tb = sp_data_pack_lup(lookup_r4), 
                  match_value_xx = ., match_var_nm_1L_chr = "name", 
                  target_var_nm_1L_chr = "source_reference", evaluate_1L_lgl = FALSE) %>% 
                ifelse(stringr::str_detect(., "::"), ., paste0("readRDS(\"", 
                  processed_fls_dir_1L_chr, "/", ., ".rds\")")))
        }
    }
}
#' Get model end ymdhs
#' @description get_model_end_ymdhs() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get model end ymdhs. Function argument input_ls specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param input_ls Input (a list)
#' @return NULL
#' @rdname get_model_end_ymdhs
#' @export 
#' @importFrom lubridate years weeks days hours minutes seconds
get_model_end_ymdhs <- function (input_ls) 
{
    input_ls$model_start_ymdhms + lubridate::years(input_ls$simulation_steps_ymwd[1]) * 
        input_ls$nbr_steps_start_to_end + months(input_ls$simulation_steps_ymwd[2]) * 
        input_ls$nbr_steps_start_to_end + lubridate::weeks(input_ls$simulation_steps_ymwd[3]) * 
        input_ls$nbr_steps_start_to_end + lubridate::days(input_ls$simulation_steps_ymwd[4]) * 
        input_ls$nbr_steps_start_to_end + lubridate::hours(input_ls$simulation_steps_ymwd[5]) * 
        input_ls$nbr_steps_start_to_end + lubridate::minutes(input_ls$simulation_steps_ymwd[6]) * 
        input_ls$nbr_steps_start_to_end + lubridate::seconds(input_ls$simulation_steps_ymwd[7]) * 
        input_ls$nbr_steps_start_to_end
}
#' Get name from path
#' @description get_name_from_path_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get name from path character vector. Function argument path_str specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param path_str PARAM_DESCRIPTION
#' @param with_ext_1L_lgl PARAM_DESCRIPTION, Default: TRUE
#' @return NULL
#' @rdname get_name_from_path_chr
#' @export 
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
get_name_from_path_chr <- function (path_str, with_ext_1L_lgl = TRUE) 
{
    if (with_ext_1L_lgl) {
        stringr::str_sub(path_str, start = stringi::stri_locate_last_regex(path_str, 
            "/")[, 2] %>% as.vector() + 1)
    }
    else {
        stringr::str_sub(path_str, start = stringi::stri_locate_last_regex(path_str, 
            "/")[, 2] %>% as.vector() + 1, end = stringi::stri_locate_last_regex(path_str, 
            "\\.")[, 2] %>% as.vector() - 1)
    }
}
#' Get non shape items for import
#' @description get_non_shape_items_for_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get non shape items for import. Function argument path_str specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param path_str PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_non_shape_items_for_imp
#' @export 
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
#' @importFrom ready4fun get_from_lup
#' @importFrom purrr map_chr
get_non_shape_items_for_imp <- function (path_str, x) 
{
    file_name <- get_name_from_path_chr(path_str)
    file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, 
        "\\.")[, 2] %>% as.vector())
    data_type <- ready4::get_from_lup_obj(data_lookup_tb = x, 
        match_value_xx = file_name, match_var_nm_1L_chr = "inc_file_main_chr", 
        target_var_nm_1L_chr = "data_type", evaluate_1L_lgl = FALSE)
    var_name_vec <- c("area_type", "main_feature", "year", "region")
    var_val_vec <- purrr::map_chr(var_name_vec, ~ready4::get_from_lup_obj(data_lookup_tb = get_menu_of_type_detail_for_imp(data_type, 
        x = x), match_value_xx = file_name, match_var_nm_1L_chr = "inc_file_main_chr", 
        target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
    make_import_object(x, var_val_vec = var_val_vec, path_str = path_str)
}
#' Get popl var prefix
#' @description get_featured_var_pfx_1L_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get popl var prefix. Function argument dynamic_var_rsl_1L_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param dynamic_var_rsl_1L_chr PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION, Default: NULL
#' @param data_year PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_featured_var_pfx_1L_chr
#' @export 

get_featured_var_pfx_1L_chr <- function (dynamic_var_rsl_1L_chr, tot_pop_resolution = NULL, 
    data_year) 
{
    if (!is.null(tot_pop_resolution)) {
        nse_names_ls <- make_nse_objs_ls(sp_unit = tot_pop_resolution, 
            concept = "tot_pop", tot_pop_col = paste0("year_", 
                data_year, "pr"), grouping_1 = dynamic_var_rsl_1L_chr, 
            data_year = data_year)
    }
    else {
        nse_names_ls <- make_nse_objs_ls(sp_unit = dynamic_var_rsl_1L_chr, 
            concept = "age_sex", grouping_1 = dynamic_var_rsl_1L_chr, 
            data_year = data_year)
    }
    paste0(nse_names_ls$popl_inc_unit, "_")
}
#' Get r import path
#' @description get_r_import_path_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get r import path character vector. Function argument r_data_dir_chr specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param r_data_dir_chr R data directory (a character vector)
#' @param name_chr Name (a character vector)
#' @param data_type_chr Data type (a character vector)
#' @return NULL
#' @rdname get_r_import_path_chr
#' @export 

get_r_import_path_chr <- function (r_data_dir_chr, name_chr, data_type_chr) 
{
    if (data_type_chr == "Geometry") 
        name_chr <- paste0(name_chr, "_sf")
    paste0(r_data_dir_chr, "/", name_chr, ".RDS")
}
#' Get res specific vars
#' @description get_res_specific_vars() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get res specific vars. Function argument var_names specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param var_names PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param featured_var_pfx_1L_chr PARAM_DESCRIPTION
#' @return NA ()
#' @rdname get_res_specific_vars
#' @export 

get_res_specific_vars <- function (var_names, data_type, data_year, featured_var_pfx_1L_chr) 
{
    if (data_type == "age_sex") {
        res_sp_vars <- var_names[var_names %>% startsWith("AREASQKM") | 
            var_names %>% startsWith(paste0("y", data_year, ".Females.")) | 
            var_names %>% startsWith(paste0("y", data_year, ".Males.")) | 
            var_names %>% startsWith(paste0("y", data_year, ".total")) | 
            var_names %>% startsWith("seifa.percentile")]
    }
    if (data_type == "tot_pop") {
        res_sp_vars <- var_names[var_names %>% startsWith("year_")]
    }
    if (data_type == "processed_age_sex") {
        res_sp_vars <- var_names[var_names %>% startsWith("pop_sp_unit_area") | 
            var_names %>% startsWith(featured_var_pfx_1L_chr)]
    }
    return(res_sp_vars)
}
#' Get resolution hierarchy
#' @description get_resolution_hierarchy() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get resolution hierarchy. Function argument data_year specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param data_year PARAM_DESCRIPTION
#' @param resolution_lup_r3 Resolution (a ready4 S3 extension of lookup table)
#' @param whole_area PARAM_DESCRIPTION, Default: TRUE
#' @return NULL
#' @rdname get_resolution_hierarchy
#' @export 
#' @importFrom dplyr filter arrange desc pull
get_resolution_hierarchy <- function (data_year, resolution_lup_r3, whole_area = TRUE) 
{
    resolution_hierarchy <- resolution_lup_r3 %>% dplyr::filter(boundary_year == 
        data_year)
    if (whole_area) {
        resolution_hierarchy <- resolution_hierarchy %>% dplyr::filter(complete == 
            TRUE)
    }
    resolution_hierarchy %>% dplyr::arrange(dplyr::desc(area_count)) %>% 
        dplyr::pull(area_type)
}
#' Get set diff lon lat
#' @description get_set_diff_lon_lat_sf() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get set diff lon lat simple features object. Function argument profile_sf specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param profile_sf Profile (a simple features object)
#' @param cut_sf Cut (a simple features object)
#' @param crs_nbr_dbl PARAM_DESCRIPTION
#' @param validate_lgl Validate (a logical vector), Default: T
#' @param min_poly_area_dbl Min poly area (a double vector), Default: units::set_units(0.05, km^2)
#' @return NULL
#' @rdname get_set_diff_lon_lat_sf
#' @export 
#' @importFrom units set_units
#' @importFrom sf st_difference st_transform st_union st_cast st_area
#' @importFrom dplyr mutate n pull filter select
#' @importFrom purrr map map_dfr
get_set_diff_lon_lat_sf <- function (profile_sf, cut_sf, crs_nbr_dbl, validate_lgl = T, 
    min_poly_area_dbl = units::set_units(0.05, km^2)) 
{
    new_sf <- sf::st_difference(profile_sf %>% sf::st_transform(crs = crs_nbr_dbl[2]), 
        sf::st_union(cut_sf) %>% sf::st_transform(crs = crs_nbr_dbl[2])) %>% 
        sf::st_transform(crs = crs_nbr_dbl[1])
    if (validate_lgl) 
        new_sf <- new_sf %>% make_valid_new_sf()
    new_sf <- new_sf %>% dplyr::mutate(feature_idx_int = 1:dplyr::n())
    new_ls <- purrr::map(dplyr::pull(new_sf, feature_idx_int), 
        ~new_sf %>% dplyr::filter(feature_idx_int == .x) %>% 
            sf::st_cast("POLYGON") %>% dplyr::mutate(new_area = sf::st_area(.)) %>% 
            dplyr::filter(new_area > units::set_units(0.05, km^2)) %>% 
            sf::st_cast() %>% dplyr::select(-new_area, -feature_idx_int))
    if (length(new_ls) > 1) {
        purrr::map_dfr(new_ls, ~.x)
    }
    else {
        new_ls[[1]]
    }
}
#' Get sngl path for import
#' @description get_sngl_path_for_imp() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get sngl path for import. Function argument downloaded_data_tb specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param downloaded_data_tb Downloaded data (a tibble)
#' @param match_value_xx PARAM_DESCRIPTION
#' @param data_directory PARAM_DESCRIPTION
#' @return NULL
#' @rdname get_sngl_path_for_imp
#' @export 
#' @importFrom purrr map_chr
#' @importFrom dplyr select
#' @importFrom ready4fun get_from_lup
get_sngl_path_for_imp <- function (downloaded_data_tb, match_value_xx, data_directory) 
{
    path_element_vector <- purrr::map_chr(downloaded_data_tb %>% 
        dplyr::select(-name) %>% names(), ~ready4::get_from_lup_obj(data_lookup_tb = downloaded_data_tb, 
        match_var_nm_1L_chr = "name", match_value_xx = match_value_xx, 
        target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
    paste0(data_directory, "/", paste(path_element_vector, collapse = "/"))
}
#' Get spatial data list
#' @description get_spatial_data_list() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get spatial data list. Function argument input_ls specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param input_ls Input (a list)
#' @param sub_div_unit PARAM_DESCRIPTION, Default: NULL
#' @param require_year_match PARAM_DESCRIPTION, Default: TRUE
#' @param excl_diff_bound_yr PARAM_DESCRIPTION, Default: TRUE
#' @return NA ()
#' @rdname get_spatial_data_list
#' @export 
#' @importFrom stringr str_sub
#' @importFrom purrr map map_chr map2 map_lgl prepend
#' @importFrom stats setNames
#' @importFrom dplyr filter pull
#' @importFrom ready4fun get_from_lup
get_spatial_data_list <- function (input_ls, sub_div_unit = NULL, require_year_match = TRUE, 
    excl_diff_bound_yr = TRUE) 
{
    attributes_to_import <- get_spatial_data_names(input_ls = input_ls, 
        sub_div_unit = sub_div_unit, require_year_match = require_year_match, 
        excl_diff_bound_yr = excl_diff_bound_yr)
    boundary_res <- stringr::str_sub(attributes_to_import, 5, 
        7) %>% unique() %>% toupper()
    data_names_list <- purrr::map(boundary_res, ~attributes_to_import[stringr::str_sub(attributes_to_import, 
        5, 7) == tolower(.x)]) %>% stats::setNames(boundary_res)
    year_vec <- make_year_vec(input_ls = input_ls)
    extra_names <- purrr::map(input_ls$at_specified_res, ~lookup_tb(input_ls$pa_r4) %>% 
        sp_data_pack_lup() %>% dplyr::filter(main_feature == 
        .x[1]) %>% dplyr::filter(make_year_filter_logic_vec(data_tb = ., 
        included_years_vec = year_vec)) %>% ready4::get_from_lup_obj(match_value_xx = .x[1], 
        match_var_nm_1L_chr = "main_feature", target_var_nm_1L_chr = "name", 
        evaluate_1L_lgl = FALSE)) %>% stats::setNames(purrr::map_chr(input_ls$at_specified_res, 
        ~.x[2]))
    res_to_merge <- names(extra_names)[names(extra_names) %in% 
        boundary_res]
    if (!identical(res_to_merge, character(0))) {
        merged_elements_ls <- purrr::map2(data_names_list[res_to_merge], 
            extra_names[res_to_merge], ~c(.x, .y))
        if (length(merged_elements_ls) == length(data_names_list)) {
            data_names_list <- merged_elements_ls
        }
        else {
            data_names_list <- append(data_names_list[names(data_names_list)[!names(data_names_list) %in% 
                res_to_merge]], merged_elements_ls)
        }
    }
    extra_res <- names(extra_names)[!names(extra_names) %in% 
        boundary_res]
    if (!identical(extra_res, character(0))) {
        data_names_list <- append(data_names_list, extra_names[extra_res])
        boundary_res <- c(boundary_res, extra_res)
    }
    data_sf_list <- purrr::map2(boundary_res, data_names_list, 
        ~add_attr_recrly_to_sf(input_ls = input_ls, sub_div_unit = sub_div_unit, 
            area_unit_1L_chr = .x, boundary_year = lookup_tb(input_ls$pa_r4) %>% 
                sp_data_pack_lup() %>% dplyr::filter(name %in% 
                .y) %>% dplyr::pull(year) %>% min(as.numeric()), 
            attribute_data = .y)) %>% stats::setNames(boundary_res)
    index_ppr <- purrr::map_lgl(data_names_list, ~check_if_ppr(.x, 
        data_lookup_tb = lookup_tb(input_ls$pa_r4) %>% sp_data_pack_lup(), 
        pop_projs_str = input_ls$pop_projs_str)) %>% which() + 
        1
    data_sf_list <- purrr::prepend(data_sf_list, list(index_ppr = index_ppr))
    return(data_sf_list)
}
#' Get spatial data names
#' @description get_spatial_data_names() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get spatial data names. Function argument input_ls specifies the where to look for the required object. The function is called for its side effects and does not return a value.
#' @param input_ls Input (a list)
#' @param sub_div_unit PARAM_DESCRIPTION, Default: NULL
#' @param require_year_match PARAM_DESCRIPTION, Default: TRUE
#' @param excl_diff_bound_yr PARAM_DESCRIPTION, Default: TRUE
#' @return NULL
#' @rdname get_spatial_data_names
#' @export 
#' @importFrom purrr map map_chr map2 flatten_chr map2_chr map_dbl reduce
#' @importFrom dplyr filter pull
#' @importFrom ready4fun get_from_lup
get_spatial_data_names <- function (input_ls, sub_div_unit = NULL, require_year_match = TRUE, 
    excl_diff_bound_yr = TRUE) 
{
    at_highest_res <- input_ls$at_highest_res
    data_year <- data_year(input_ls$pa_r4)
    at_specified_res <- input_ls$at_specified_res
    country <- country(input_ls$pa_r4)
    pop_projs_str <- input_ls$pop_projs_str
    lookup_tb_r4 <- input_ls$pa_r4 %>% lookup_tb()
    spatial_lookup_tb <- sp_data_pack_lup(lookup_tb_r4)
    abbreviations_lookup_tb <- sp_abbreviations_lup(lookup_tb_r4)
    year_vec <- make_year_vec(input_ls = input_ls)
    lookup_tb_list <- purrr::map(at_highest_res, ~spatial_lookup_tb %>% 
        dplyr::filter(main_feature == .x) %>% dplyr::filter(year %in% 
        year_vec[if (.x == pop_projs_str) 
            1:length(year_vec)
        else 1]))
    data_res_vec <- purrr::map_chr(lookup_tb_list, ~.x %>% dplyr::pull(area_type) %>% 
        unique() %>% get_highest_res(year = data_year, resolution_lup_r3 = sp_resolution_lup(lookup_tb_r4)))
    data_unavail_for_year <- is.na(data_res_vec)
    if (require_year_match & sum(data_unavail_for_year) > 0) 
        stop("Data not available for specified year for all data requested")
    matched_year_vec <- at_highest_res[!data_unavail_for_year]
    matched_yr_lookup_tb_list <- lookup_tb_list[!data_unavail_for_year]
    matched_yr_data_res_vec <- data_res_vec[!data_unavail_for_year]
    non_matched_year_vec <- at_highest_res[is.na(data_res_vec)]
    matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list, 
        matched_yr_data_res_vec, ~.x %>% dplyr::filter(area_type == 
            .y))
    names_of_data_vec <- purrr::map(matched_yr_lookup_tb_list, 
        ~.x %>% dplyr::pull(name)) %>% purrr::flatten_chr()
    if (!identical(non_matched_year_vec, character(0))) {
        closest_years <- get_closest_year(data_lookup_tb = spatial_lookup_tb, 
            inc_main_ft_vec = non_matched_year_vec, target_year = data_year)
        extra_names <- purrr::map2_chr(non_matched_year_vec, 
            closest_years, ~ready4::get_from_lup_obj(data_lookup_tb = spatial_lookup_tb %>% 
                dplyr::filter(year == .y), match_value_xx = .x, 
                match_var_nm_1L_chr = "main_feature", target_var_nm_1L_chr = "name", 
                evaluate_1L_lgl = FALSE))
        non_matched_positions <- purrr::map_dbl(non_matched_year_vec, 
            ~which(at_highest_res == .x))
        names_of_data_vec <- purrr::reduce(1:length(non_matched_positions), 
            .init = names_of_data_vec, ~append(.x, extra_names[.y], 
                after = non_matched_positions[.y] - 1))
    }
    names_of_data_vec
}
#' Get starter simple features object for profiled area
#' @description get_starter_sf_for_profiled_area() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get starter simple features object for profiled area. Function argument pa_r4 specifies the where to look for the required object. The function returns Starter (a simple features object).
#' @param pa_r4 Pa (a ready4 S4)
#' @param group_by_var_1L_chr PARAM_DESCRIPTION
#' @return Starter (a simple features object)
#' @rdname get_starter_sf_for_profiled_area
#' @export 
#' @importFrom dplyr filter
#' @importFrom ready4fun get_from_lup
#' @importFrom stringr str_sub
#' @importFrom sf `st_crs<-`
#' @importFrom rlang sym
get_starter_sf_for_profiled_area <- function (pa_r4, group_by_var_1L_chr) 
{
    sp_data_starter_sf_lup <- pa_r4 %>% lookup_tb() %>% sp_starter_sf_lup() %>% 
        dplyr::filter(country == country(pa_r4))
    if (!is.na(area_bound_year(pa_r4))) 
        sp_data_starter_sf_lup <- sp_data_starter_sf_lup %>% 
            dplyr::filter(area_bound_yr == area_bound_year(pa_r4))
    starter_sf_nm <- ready4::get_from_lup_obj(data_lookup_tb = sp_data_starter_sf_lup, 
        match_var_nm_1L_chr = "area_type", match_value_xx = ifelse(area_type(pa_r4) %in% 
            sp_data_starter_sf_lup$area_type, area_type(pa_r4), 
            region_type(pa_r4)), target_var_nm_1L_chr = "starter_sf", 
        evaluate_1L_lgl = FALSE)
    starter_sf <- procure(pa_r4 %>% lookup_tb() %>% sp_data_pack_lup(), 
        col_nm_1L_chr = "name", value_chr = starter_sf_nm %>% stringr::str_sub(end = -4))
    if (use_coord_lup(pa_r4)) {
        starter_sf <- starter_sf %>% sf::`st_crs<-`(crs_nbr(pa_r4)[1])
    }
    else {
        starter_sf <- starter_sf %>% dplyr::filter(!!rlang::sym(group_by_var_1L_chr) %in% 
            features(pa_r4))
    }
    return(starter_sf)
}
#' Get sys data tibbles
#' @description get_sys_data_tbs_ls() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get sys data tibbles list. The function is called for its side effects and does not return a value.

#' @return NULL
#' @rdname get_sys_data_tbs_ls
#' @export 

get_sys_data_tbs_ls <- function () 
{
    list(aus_spatial_lookup_tb = aus_spatial_lookup_tb, aus_data_resolution_tb = aus_data_resolution_tb, 
        aus_state_short_tb = aus_state_short_tb, group_by_var_1L_chr_lookup_tb = group_by_var_1L_chr_lookup_tb)
}
