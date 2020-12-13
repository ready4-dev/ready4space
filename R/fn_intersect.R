#' Intersect lon lat sfs
#' @description intersect_lon_lat_sfs() is an Intersect function that gets the intersection between two or more data objects. Specifically, this function implements an algorithm to intersect lon lat sfs. The function is called for its side effects and does not return a value.
#' @param sf_1 PARAM_DESCRIPTION
#' @param sf_2 PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @param validate_lgl Validate (a logical vector), Default: T
#' @return NULL
#' @rdname intersect_lon_lat_sfs
#' @export 
#' @importFrom sf st_intersection st_transform
intersect_lon_lat_sfs <- function (sf_1, sf_2, crs_nbr_vec, validate_lgl = T) 
{
    sf_3 <- sf::st_intersection(sf_1 %>% sf::st_transform(crs_nbr_vec[2]), 
        sf_2 %>% sf::st_transform(crs_nbr_vec[2])) %>% sf::st_transform(crs_nbr_vec[1])
    if (validate_lgl) 
        sf_3 %>% make_valid_new_sf()
    else sf_3
}
#' Intersect sfs keep counts
#' @description intersect_sfs_keep_counts() is an Intersect function that gets the intersection between two or more data objects. Specifically, this function implements an algorithm to intersect sfs keep counts. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param profiled_colref PARAM_DESCRIPTION, Default: NA
#' @param profiled_rowref PARAM_DESCRIPTION, Default: NA
#' @param attribute_sf Attribute (a simple features object)
#' @param attribute_unit PARAM_DESCRIPTION
#' @param data_type PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION, Default: NULL
#' @return Profiled (a simple features object)
#' @rdname intersect_sfs_keep_counts
#' @export 
#' @importFrom dplyr select pull slice
#' @importFrom stringr str_which
intersect_sfs_keep_counts <- function (profiled_sf, profiled_colref = NA, profiled_rowref = NA, 
    attribute_sf, attribute_unit, data_type, data_year, crs_nbr_vec, 
    popl_var_prefix = NULL) 
{
    if (!is.na(profiled_colref)) {
        if (!is.na(profiled_rowref)) {
            profiled_sf <- profiled_sf %>% dplyr::select(!!profiled_colref)
            index.nbr <- profiled_sf %>% dplyr::pull(profiled_colref) %>% 
                stringr::str_which(profiled_rowref)
            profiled_sf <- profiled_sf %>% dplyr::slice(index.nbr)
        }
        else profiled_sf <- profiled_sf %>% dplyr::select(!!profiled_colref)
    }
    attribute_sf <- rename_vars_based_on_res(sf = attribute_sf, 
        data_type = data_type, data_year = data_year, popl_var_prefix = popl_var_prefix, 
        feature_nm = attribute_unit)
    profiled_sf <- intersect_lon_lat_sfs(sf_1 = profiled_sf, 
        sf_2 = attribute_sf, crs_nbr_vec = crs_nbr_vec)
    return(profiled_sf)
}
#' Intersect sfs update counts
#' @description intersect_sfs_update_counts() is an Intersect function that gets the intersection between two or more data objects. Specifically, this function implements an algorithm to intersect sfs update counts. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param profiled_colref PARAM_DESCRIPTION, Default: NA
#' @param profiled_rowref PARAM_DESCRIPTION, Default: NA
#' @param sp_data_list PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param age_sex_counts_grouped_by PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @return Profiled (a simple features object)
#' @rdname intersect_sfs_update_counts
#' @export 
#' @importFrom sf st_set_geometry
#' @importFrom dplyr distinct select ends_with rename_at vars
#' @importFrom stringi stri_replace_last_regex
intersect_sfs_update_counts <- function (profiled_sf, profiled_colref = NA, profiled_rowref = NA, 
    sp_data_list, tot_pop_resolution, age_sex_pop_resolution, 
    group_by_var, age_sex_counts_grouped_by, data_year, crs_nbr_vec) 
{
    if (!is.null(tot_pop_resolution)) {
        if (age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])) {
            sp_data_list[[age_sex_pop_resolution]] <- merge(sp_data_list[[tot_pop_resolution]], 
                sf::st_set_geometry(sp_data_list[[age_sex_pop_resolution]], 
                  NULL), by = age_sex_counts_grouped_by) %>% 
                dplyr::distinct(.keep_all = T) %>% dplyr::select(-dplyr::ends_with(".x")) %>% 
                dplyr::rename_at(.vars = dplyr::vars(dplyr::ends_with(".y")), 
                  ~stringi::stri_replace_last_regex(.x, "\\.y$", 
                    ""))
            sp_data_list[[age_sex_pop_resolution]] <- rename_vars_based_on_res(sf = sp_data_list[[age_sex_pop_resolution]], 
                data_type = "tot_pop", data_year = data_year, 
                feature_nm = tot_pop_resolution) %>% add_kmsq_area_all_features(feature_nm = tot_pop_resolution)
        }
    }
    sp_data_list[[age_sex_pop_resolution]] <- sp_data_list[[age_sex_pop_resolution]] %>% 
        add_kmsq_area_by_group(group_by_var = age_sex_counts_grouped_by, 
            feature_nm = age_sex_pop_resolution)
    profiled_sf <- intersect_sfs_keep_counts(profiled_sf = profiled_sf, 
        profiled_colref = profiled_colref, profiled_rowref = profiled_rowref, 
        attribute_sf = sp_data_list[[age_sex_pop_resolution]], 
        attribute_unit = age_sex_pop_resolution, data_type = "age_sex", 
        data_year = data_year, crs_nbr_vec = crs_nbr_vec)
    if (!is.null(tot_pop_resolution)) {
        if (!age_sex_counts_grouped_by %in% names(sp_data_list[[tot_pop_resolution]])) {
            profiled_sf <- intersect_sfs_keep_counts(profiled_sf = profiled_sf, 
                profiled_colref = profiled_colref, profiled_rowref = profiled_rowref, 
                attribute_sf = sp_data_list[[tot_pop_resolution]] %>% 
                  add_kmsq_area_all_features(feature_nm = tot_pop_resolution), 
                attribute_unit = tot_pop_resolution, data_type = "tot_pop")
        }
    }
    profiled_sf <- update_pop_count_by_areas(profiled_sf = profiled_sf, 
        group_by_var = group_by_var, age_sex_var_name = age_sex_counts_grouped_by, 
        data_year = data_year, age_sex_pop_resolution = age_sex_pop_resolution, 
        tot_pop_resolution = tot_pop_resolution)
    return(profiled_sf)
}
