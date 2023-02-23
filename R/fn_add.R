#' Add attribute to simple features object
#' @description add_att_to_sf() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add attribute to simple features object. Function argument area_sf specifies the object to be updated. The function returns Updated area (a simple features object).
#' @param area_sf Area (a simple features object)
#' @param att_data_tb Attribute data (a tibble)
#' @param att_data_desc_1L_chr Attribute data description (a character vector of length one)
#' @return Updated area (a simple features object)
#' @rdname add_att_to_sf
#' @export 
#' @importFrom dplyr inner_join
#' @importFrom stringr str_detect
#' @importFrom sf st_as_sf
#' @keywords internal
add_att_to_sf <- function (area_sf, att_data_tb, att_data_desc_1L_chr) 
{
    if (att_data_desc_1L_chr == "PPR") {
        updated_area_sf <- dplyr::inner_join(area_sf, att_data_tb)
    }
    if (stringr::str_detect(att_data_desc_1L_chr, "ERP_TOT")) {
        updated_area_sf <- dplyr::inner_join(area_sf, att_data_tb) %>% 
            sf::st_as_sf()
    }
    if (att_data_desc_1L_chr == "ERP_ASX") {
        updated_area_sf <- dplyr::inner_join(area_sf, att_data_tb) %>% 
            sf::st_as_sf()
    }
    return(updated_area_sf)
}
#' Add dynamic variables to simple features object
#' @description add_dynamic_vars_to_sf() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add dynamic variables to simple features object. Function argument dynamic_vars_sf specifies the object to be updated. The function returns Profiled (a simple features object).
#' @param dynamic_vars_sf Dynamic variables (a simple features object)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param dynamic_var_nm_1L_chr Dynamic variable name (a character vector of length one)
#' @param dynamic_var_rsl_1L_chr Dynamic variable resolution (a character vector of length one)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one)
#' @param profiled_sf Profiled (a simple features object)
#' @param reference_vals_chr Reference values (a character vector)
#' @return Profiled (a simple features object)
#' @rdname add_dynamic_vars_to_sf
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @keywords internal
add_dynamic_vars_to_sf <- function (dynamic_vars_sf, crs_nbr_dbl, data_year_1L_chr, dynamic_var_nm_1L_chr, 
    dynamic_var_rsl_1L_chr, featured_var_pfx_1L_chr, profiled_sf, 
    reference_vals_chr) 
{
    profiled_sf <- make_intersecting_profiled_area(attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
        attribute_sf = profiled_sf, crs_nbr_dbl = crs_nbr_dbl, 
        data_type_chr = "processed_age_sex", data_year_1L_chr = data_year_1L_chr, 
        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, profiled_sf = dynamic_vars_sf, 
        profiled_sf_col_1L_chr = NA_character_, profiled_sf_row_1L_chr = NA_character_) %>% 
        add_km_sqd_by_group(group_by_var_1L_chr = dynamic_var_nm_1L_chr, 
            feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
    dyn_param_unit_id_1L_chr <- names(dynamic_vars_sf)[1]
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(dynamic_var_nm_1L_chr), 
        paste0(!!rlang::sym(dyn_param_unit_id_1L_chr), "_", !!rlang::sym(dynamic_var_nm_1L_chr))))
    profiled_sf <- update_popl_counts(profiled_sf = profiled_sf, 
        group_by_var_1L_chr = group_by_var_1L_chr, dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr, 
        data_year_1L_chr = data_year_1L_chr, dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
        reference_var_rsl_1L_chr = NULL, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, 
        reference_vals_chr = reference_vals_chr)
    return(profiled_sf)
}
#' Add kilometre squared
#' @description add_km_sqd() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add kilometre squared. Function argument geometry_sf specifies the object to be updated. The function returns Geometry (a simple features object).
#' @param geometry_sf Geometry (a simple features object)
#' @param feature_nm_1L_chr Feature name (a character vector of length one)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'whl_'
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: '_area'
#' @return Geometry (a simple features object)
#' @rdname add_km_sqd
#' @export 
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
#' @keywords internal
add_km_sqd <- function (geometry_sf, feature_nm_1L_chr, prefix_1L_chr = "whl_", 
    suffix_1L_chr = "_area") 
{
    geometry_sf <- geometry_sf %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(prefix_1L_chr, 
        feature_nm_1L_chr, suffix_1L_chr)), sf::st_area(.) %>% 
        units::set_units(km^2)))
    return(geometry_sf)
}
#' Add kilometre squared by group
#' @description add_km_sqd_by_group() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add kilometre squared by group. Function argument geometry_sf specifies the object to be updated. The function returns Geometry (a simple features object).
#' @param geometry_sf Geometry (a simple features object)
#' @param feature_nm_1L_chr Feature name (a character vector of length one)
#' @param group_by_var_1L_chr Group by variable (a character vector of length one)
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'whl_'
#' @param suffix_1L_chr Suffix (a character vector of length one), Default: '_area'
#' @return Geometry (a simple features object)
#' @rdname add_km_sqd_by_group
#' @export 
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom rlang sym
#' @importFrom sf st_combine st_set_geometry
#' @keywords internal
add_km_sqd_by_group <- function (geometry_sf, feature_nm_1L_chr, group_by_var_1L_chr, 
    prefix_1L_chr = "whl_", suffix_1L_chr = "_area") 
{
    geometry_sf <- merge(geometry_sf, geometry_sf %>% dplyr::group_by(!!rlang::sym(group_by_var_1L_chr)) %>% 
        dplyr::summarise(geometry = sf::st_combine(geometry)) %>% 
        add_km_sqd(feature_nm_1L_chr = feature_nm_1L_chr, prefix_1L_chr = prefix_1L_chr, 
            suffix_1L_chr = suffix_1L_chr) %>% dplyr::ungroup() %>% 
        sf::st_set_geometry(NULL))
    return(geometry_sf)
}
#' Add names
#' @description add_names() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add names. Function argument ds_tb specifies the object to be updated. The function returns Dataset (a tibble).
#' @param ds_tb Dataset (a tibble)
#' @return Dataset (a tibble)
#' @rdname add_names
#' @export 
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_chr
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stringr str_sub
#' @keywords internal
add_names <- function (ds_tb) 
{
    data(ISO_3166_1, package = "ISOcodes", envir = environment())
    ds_tb <- ds_tb %>% dplyr::mutate(name_chr = purrr::pmap_chr(list(country_chr, 
        area_type_chr, region_chr, data_type_chr, main_feature_chr, 
        year_chr), ~paste0(ready4::get_from_lup_obj(data_lookup_tb = ISO_3166_1, 
        match_value_xx = ..1, match_var_nm_1L_chr = "Name", target_var_nm_1L_chr = "Alpha_3", 
        evaluate_1L_lgl = FALSE) %>% tolower(), "_", tolower(..2), 
        "_", tolower(..3 %>% stringr::str_sub(end = 3)), ifelse(..4 == 
            "Geometry", ifelse(..5 == "Boundary", "_bnd_", "_crd_"), 
            paste0("_", tolower(..5), "_")), ..6)))
    ds_tb <- ds_tb %>% dplyr::mutate(name_chr = make.unique(name_chr)) %>% 
        dplyr::mutate(name_chr = map_chr(name_chr, ~ifelse(stringr::str_sub(.x, 
            start = -2, end = -2) == ".", paste0(stringr::str_sub(.x, 
            end = 11), stringr::str_sub(.x, start = -1), stringr::str_sub(.x, 
            start = 12, end = -3)), .x)))
    return(ds_tb)
}
#' Add population counts
#' @description add_popl_counts() is an Add function that updates an object by adding data to that object. Specifically, this function implements an algorithm to add population counts. Function argument profiled_sf specifies the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param group_by_var_1L_chr Group by variable (a character vector of length one)
#' @param nse_objs_ls Non-standard evaluation objects (a list)
#' @param convert_sfx_to_pfx_1L_lgl Convert suffix to prefix (a logical vector of length one), Default: FALSE
#' @param top_level_1L_lgl Top level (a logical vector of length one), Default: FALSE
#' @return Profiled (a simple features object)
#' @rdname add_popl_counts
#' @export 
#' @importFrom sf st_set_geometry
#' @importFrom dplyr group_by summarise_at vars starts_with funs ungroup bind_cols select inner_join rename_at
#' @importFrom rlang sym
#' @keywords internal
add_popl_counts <- function (profiled_sf, group_by_var_1L_chr, nse_objs_ls, convert_sfx_to_pfx_1L_lgl = FALSE, 
    top_level_1L_lgl = FALSE) 
{
    group_totals <- profiled_sf %>% sf::st_set_geometry(NULL) %>% 
        dplyr::group_by(!!rlang::sym(grp_var_name)) %>% dplyr::summarise_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)), 
        dplyr::funs(`:=`(!!rlang::sym(nse_objs_ls$grouping_1_concept_tot), 
            sum(.))))
    if (convert_sfx_to_pfx_1L_lgl) 
        group_totals <- group_totals %>% transform_sfx_to_pfx(suffix_1L_chr = nse_objs_ls$grouping_1_concept_tot)
    group_totals <- group_totals %>% dplyr::ungroup()
    if (top_level_1L_lgl) {
        profiled_sf <- dplyr::bind_cols(profiled_sf, group_totals[rep(row.names(group_totals), 
            nrow(profiled_sf)), ] %>% dplyr::select(-!!rlang::sym(grp_var_name)))
    }
    else {
        profiled_sf <- profiled_sf %>% dplyr::inner_join(., group_totals %>% 
            dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$grouping_1_concept_tot)), 
                dplyr::funs(gsub(paste0(nse_objs_ls$popl_inc_unit, 
                  "_"), "", .))))
    }
    return(profiled_sf)
}
