#' Intersect lon lat sfs
#' @description make_intersecting_geometries() is an Intersect function that gets the intersection between two or more data objects. Specifically, this function implements an algorithm to intersect lon lat sfs. The function is called for its side effects and does not return a value.
#' @param sf_1 PARAM_DESCRIPTION
#' @param sf_2 PARAM_DESCRIPTION
#' @param crs_nbr_dbl PARAM_DESCRIPTION
#' @param validate_lgl Validate (a logical vector), Default: T
#' @return NULL
#' @rdname make_intersecting_geometries
#' @export 
#' @importFrom sf st_intersection st_transform
make_intersecting_geometries <- function (sf_1, sf_2, crs_nbr_dbl, validate_lgl = T) 
{
    sf_3 <- sf::st_intersection(sf_1 %>% sf::st_transform(crs_nbr_dbl[2]), 
        sf_2 %>% sf::st_transform(crs_nbr_dbl[2])) %>% sf::st_transform(crs_nbr_dbl[1])
    if (validate_lgl) 
        sf_3 %>% make_valid_new_sf()
    else sf_3
}
#' Intersect sfs keep counts
#' @description make_intersecting_profiled_area() is an Intersect function that gets the intersection between two or more data objects. Specifically, this function implements an algorithm to intersect sfs keep counts. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param profiled_sf_col_1L_chr PARAM_DESCRIPTION, Default: NA
#' @param profiled_sf_row_1L_chr PARAM_DESCRIPTION, Default: NA
#' @param attribute_sf Attribute (a simple features object)
#' @param attribute_rsl_1L_chr PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_dbl PARAM_DESCRIPTION
#' @param featured_var_pfx_1L_chr PARAM_DESCRIPTION, Default: NULL
#' @return Profiled (a simple features object)
#' @rdname make_intersecting_profiled_area
#' @export 
#' @importFrom dplyr select pull slice
#' @importFrom stringr str_which
make_intersecting_profiled_area <- function (profiled_sf, profiled_sf_col_1L_chr = NA, profiled_sf_row_1L_chr = NA, 
    attribute_sf, attribute_rsl_1L_chr, data_type, data_year, crs_nbr_dbl, 
    featured_var_pfx_1L_chr = NULL) 
{
    if (!is.na(profiled_sf_col_1L_chr)) {
        if (!is.na(profiled_sf_row_1L_chr)) {
            profiled_sf <- profiled_sf %>% dplyr::select(!!profiled_sf_col_1L_chr)
            index.nbr <- profiled_sf %>% dplyr::pull(profiled_sf_col_1L_chr) %>% 
                stringr::str_which(profiled_sf_row_1L_chr)
            profiled_sf <- profiled_sf %>% dplyr::slice(index.nbr)
        }
        else profiled_sf <- profiled_sf %>% dplyr::select(!!profiled_sf_col_1L_chr)
    }
    attribute_sf <- rename_vars_based_on_res(sf = attribute_sf, 
        data_type = data_type, data_year = data_year, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, 
        feature_nm_1L_chr = attribute_rsl_1L_chr)
    profiled_sf <- make_intersecting_geometries(sf_1 = profiled_sf, 
        sf_2 = attribute_sf, crs_nbr_dbl = crs_nbr_dbl)
    return(profiled_sf)
}
#' Intersect sfs update counts
#' @description make_reconciled_intersecting_area() is an Intersect function that gets the intersection between two or more data objects. Specifically, this function implements an algorithm to intersect sfs update counts. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param profiled_sf_col_1L_chr PARAM_DESCRIPTION, Default: NA
#' @param profiled_sf_row_1L_chr PARAM_DESCRIPTION, Default: NA
#' @param sp_data_list PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param dynamic_var_rsl_1L_chr PARAM_DESCRIPTION
#' @param group_by_var_1L_chr PARAM_DESCRIPTION
#' @param age_sex_counts_grouped_by PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_dbl PARAM_DESCRIPTION
#' @return Profiled (a simple features object)
#' @rdname make_reconciled_intersecting_area
#' @export 
#' @importFrom sf st_set_geometry
#' @importFrom dplyr distinct select ends_with rename_at vars
#' @importFrom stringi stri_replace_last_regex
make_reconciled_intersecting_area <- function (profiled_sf, profiled_sf_col_1L_chr = NA, profiled_sf_row_1L_chr = NA, 
    sp_data_list, tot_pop_resolution, dynamic_var_rsl_1L_chr, 
    group_by_var_1L_chr, age_sex_counts_grouped_by, data_year, crs_nbr_dbl) 
{
    if (!is.null(tot_pop_resolution)) {
        if (age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])) {
            sp_data_list[[dynamic_var_rsl_1L_chr]] <- merge(sp_data_list[[tot_pop_resolution]], 
                sf::st_set_geometry(sp_data_list[[dynamic_var_rsl_1L_chr]], 
                  NULL), by = age_sex_counts_grouped_by) %>% 
                dplyr::distinct(.keep_all = T) %>% dplyr::select(-dplyr::ends_with(".x")) %>% 
                dplyr::rename_at(.vars = dplyr::vars(dplyr::ends_with(".y")), 
                  ~stringi::stri_replace_last_regex(.x, "\\.y$", 
                    ""))
            sp_data_list[[dynamic_var_rsl_1L_chr]] <- rename_vars_based_on_res(sf = sp_data_list[[dynamic_var_rsl_1L_chr]], 
                data_type = "tot_pop", data_year = data_year, 
                feature_nm_1L_chr = tot_pop_resolution) %>% add_km_sqd(feature_nm_1L_chr = tot_pop_resolution)
        }
    }
    sp_data_list[[dynamic_var_rsl_1L_chr]] <- sp_data_list[[dynamic_var_rsl_1L_chr]] %>% 
        add_km_sqd_by_group(group_by_var_1L_chr = age_sex_counts_grouped_by, 
            feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
    profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf, 
        profiled_sf_col_1L_chr = profiled_sf_col_1L_chr, profiled_sf_row_1L_chr = profiled_sf_row_1L_chr, 
        attribute_sf = sp_data_list[[dynamic_var_rsl_1L_chr]], 
        attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr, data_type = "age_sex", 
        data_year = data_year, crs_nbr_dbl = crs_nbr_dbl)
    if (!is.null(tot_pop_resolution)) {
        if (!age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])) {
            profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf, 
                profiled_sf_col_1L_chr = profiled_sf_col_1L_chr, profiled_sf_row_1L_chr = profiled_sf_row_1L_chr, 
                attribute_sf = sp_data_list[[tot_pop_resolution]] %>% 
                  add_km_sqd(feature_nm_1L_chr = tot_pop_resolution), 
                attribute_rsl_1L_chr = tot_pop_resolution, data_type = "tot_pop")
        }
    }
    profiled_sf <- update_pop_count_by_areas(profiled_sf = profiled_sf, 
        group_by_var_1L_chr = group_by_var_1L_chr, dynamic_var_nm_1L_chr = age_sex_counts_grouped_by, 
        data_year = data_year, dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
        tot_pop_resolution = tot_pop_resolution)
    return(profiled_sf)
}
