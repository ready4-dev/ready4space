#' Update isochrone table
#' @description update_isochrone_tbl() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update isochrone table. Function argument index_val_1L_int specifies the object to be updated. Argument temporal_bands_ls provides the object to be updated. The function returns Isochrone (a tibble).
#' @param index_val_1L_int Index value (an integer vector of length one)
#' @param temporal_bands_ls Temporal bands (a list)
#' @param travel_mode_1L_chr Travel mode (a character vector of length one)
#' @return Isochrone (a tibble)
#' @rdname update_isochrone_tbl
#' @export 
#' @importFrom purrr pluck
#' @importFrom stringr str_subset str_replace_all
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
#' @keywords internal
update_isochrone_tbl <- function (index_val_1L_int, temporal_bands_ls, travel_mode_1L_chr) 
{
    max_var_1L_chr <- "isomax"
    max_vars_chr <- temporal_bands_ls %>% purrr::pluck(index_val_1L_int) %>% 
        names() %>% stringr::str_subset("isomax")
    if (length(max_vars_chr) > 1) {
        max_var_1L_chr <- max_vars_chr %>% stringr::str_subset("isomax.") %>% 
            stringr::str_replace_all("isomax.", "") %>% as.numeric() %>% 
            max() %>% paste0("isomax.", .)
    }
    isochrone_tb <- temporal_bands_ls %>% purrr::pluck(index_val_1L_int) %>% 
        dplyr::mutate(`:=`(isomax, !!rlang::sym(max_var_1L_chr))) %>% 
        dplyr::mutate(center_value = (isomin + isomax)/2) %>% 
        dplyr::mutate(`:=`(!!rlang::sym(paste0(travel_mode_1L_chr, 
            "_times")), paste0("0 to ", isomax, " mins"))) %>% 
        dplyr::select(id, isomin, isomax, center_value, !!rlang::sym(paste0(travel_mode_1L_chr, 
            "_times")))
    return(isochrone_tb)
}
#' Update population by group
#' @description update_popl_by_group() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update population by group. Function argument profiled_sf specifies the object to be updated. Argument data_year_1L_chr provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param dynamic_var_nm_1L_chr Dynamic variable name (a character vector of length one)
#' @param dynamic_var_rsl_1L_chr Dynamic variable resolution (a character vector of length one)
#' @param group_by_var_1L_chr Group by variable (a character vector of length one)
#' @param reference_var_rsl_1L_chr Reference variable resolution (a character vector of length one)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one), Default: ''
#' @param reference_vals_chr Reference values (a character vector)
#' @return Profiled (a simple features object)
#' @rdname update_popl_by_group
#' @export 
#' @keywords internal
update_popl_by_group <- function (profiled_sf, data_year_1L_chr, dynamic_var_nm_1L_chr, 
    dynamic_var_rsl_1L_chr, group_by_var_1L_chr, reference_var_rsl_1L_chr, 
    featured_var_pfx_1L_chr = "", reference_vals_chr) 
{
    profiled_sf <- add_popl_counts(profiled_sf = profiled_sf, 
        group_by_var_1L_chr = dynamic_var_nm_1L_chr, nse_objs_ls = make_nse_objs_ls(spatial_unit_1L_chr = dynamic_var_rsl_1L_chr, 
            concept_1L_chr = reference_vals_chr[2], grouping_var_1L_chr = dynamic_var_rsl_1L_chr, 
            data_year_1L_chr = data_year_1L_chr, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr), 
        convert_sfx_to_pfx_1L_lgl = TRUE)
    if (!is.null(reference_var_rsl_1L_chr)) {
        profiled_sf <- update_popl_by_incld_area(profiled_sf = profiled_sf, 
            spatial_unit_1L_chr = reference_var_rsl_1L_chr, data_year_1L_chr = data_year_1L_chr, 
            concept_1L_chr = reference_vals_chr[1], reference_var_nm_1L_chr = paste0("year_", 
                data_year_1L_chr, "pr"), dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr, 
            dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr)
    }
    if (!is.null(reference_var_rsl_1L_chr)) {
        profiled_sf <- add_popl_counts(profiled_sf = profiled_sf, 
            group_by_var_1L_chr = dynamic_var_nm_1L_chr, nse_objs_ls = make_nse_objs_ls(spatial_unit_1L_chr = reference_var_rsl_1L_chr, 
                concept_1L_chr = reference_vals_chr[1], reference_var_nm_1L_chr = paste0("year_", 
                  data_year_1L_chr, "pr"), grouping_var_1L_chr = dynamic_var_rsl_1L_chr, 
                data_year_1L_chr = data_year_1L_chr), convert_sfx_to_pfx_1L_lgl = TRUE)
    }
    profiled_sf <- add_popl_counts(profiled_sf = profiled_sf, 
        group_by_var_1L_chr = group_by_var_1L_chr, nse_objs_ls = make_nse_objs_ls(concept_1L_chr = reference_vals_chr[2], 
            spatial_unit_1L_chr = dynamic_var_rsl_1L_chr, grouping_var_1L_chr = dynamic_var_rsl_1L_chr, 
            data_year_1L_chr = data_year_1L_chr, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr), 
        convert_sfx_to_pfx_1L_lgl = TRUE, top_level_1L_lgl = TRUE)
    if (!is.null(reference_var_rsl_1L_chr)) {
        profiled_sf <- add_popl_counts(profiled_sf = profiled_sf, 
            group_by_var_1L_chr = group_by_var_1L_chr, nse_objs_ls = make_nse_objs_ls(spatial_unit_1L_chr = reference_var_rsl_1L_chr, 
                concept_1L_chr = reference_vals_chr[1], reference_var_nm_1L_chr = paste0("year_", 
                  data_year_1L_chr, "pr"), grouping_var_1L_chr = dynamic_var_rsl_1L_chr, 
                data_year_1L_chr = data_year_1L_chr), convert_sfx_to_pfx_1L_lgl = TRUE, 
            top_level_1L_lgl = TRUE)
    }
    return(profiled_sf)
}
#' Update population by included area
#' @description update_popl_by_incld_area() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update population by included area. Function argument profiled_sf specifies the object to be updated. Argument concept_1L_chr provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param concept_1L_chr Concept (a character vector of length one)
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one)
#' @param spatial_unit_1L_chr Spatial unit (a character vector of length one)
#' @param dynamic_var_nm_1L_chr Dynamic variable name (a character vector of length one), Default: NULL
#' @param dynamic_var_rsl_1L_chr Dynamic variable resolution (a character vector of length one), Default: NULL
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: NULL
#' @return Profiled (a simple features object)
#' @rdname update_popl_by_incld_area
#' @export 
#' @importFrom dplyr mutate mutate_at vars starts_with funs rename_at
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
#' @keywords internal
update_popl_by_incld_area <- function (profiled_sf, concept_1L_chr, data_year_1L_chr, featured_var_pfx_1L_chr, 
    spatial_unit_1L_chr, dynamic_var_nm_1L_chr = NULL, dynamic_var_rsl_1L_chr = NULL, 
    reference_var_nm_1L_chr = NULL) 
{
    nse_objs_ls <- make_nse_objs_ls(spatial_unit_1L_chr = spatial_unit_1L_chr, 
        concept_1L_chr = concept_1L_chr, reference_var_nm_1L_chr = reference_var_nm_1L_chr, 
        grouping_var_1L_chr = dynamic_var_rsl_1L_chr, data_year_1L_chr = data_year_1L_chr, 
        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$area_inc_unit), 
        sf::st_area(.) %>% units::set_units(km^2)))
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$prop_inc_unit), 
        as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit))))
    if (!is.null(reference_var_nm_1L_chr)) {
        profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$popl_inc_unit), 
            !!rlang::sym(nse_objs_ls$popl_whl_unit) * !!rlang::sym(nse_objs_ls$prop_inc_unit)))
        profiled_sf <- add_popl_counts(profiled_sf = profiled_sf, 
            nse_objs_ls = nse_objs_ls, group_by_var_1L_chr = dynamic_var_nm_1L_chr)
        profiled_sf <- profiled_sf %>% dplyr::mutate(pop_prop_multiplier_tot_pop = !!rlang::sym(nse_objs_ls$popl_inc_unit)/!!rlang::sym(nse_objs_ls$grouping_1_concept_tot)) %>% 
            dplyr::mutate(pop_prop_multiplier_tot_pop = ifelse(is.nan(pop_prop_multiplier_tot_pop), 
                0, pop_prop_multiplier_tot_pop))
    }
    profiled_sf <- profiled_sf %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_1), 
        dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_2)), 
        dplyr::funs(`:=`(!!rlang::sym(nse_objs_ls$popl_inc_unit), 
            . * !!rlang::sym(nse_objs_ls$popl_multiplier)))) %>% 
        transform_sfx_to_pfx(suffix_1L_chr = nse_objs_ls$popl_inc_unit) %>% 
        dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)), 
            dplyr::funs(gsub(nse_objs_ls$inc_str_to_delete, "", 
                .)))
    return(profiled_sf)
}
#' Update population counts
#' @description update_popl_counts() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update population counts. Function argument profiled_sf specifies the object to be updated. Argument data_year_1L_chr provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param dynamic_var_nm_1L_chr Dynamic variable name (a character vector of length one)
#' @param dynamic_var_rsl_1L_chr Dynamic variable resolution (a character vector of length one)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one), Default: ''
#' @param group_by_var_1L_chr Group by variable (a character vector of length one)
#' @param reference_var_rsl_1L_chr Reference variable resolution (a character vector of length one)
#' @param reference_vals_chr Reference values (a character vector)
#' @return Profiled (a simple features object)
#' @rdname update_popl_counts
#' @export 
#' @keywords internal
update_popl_counts <- function (profiled_sf, data_year_1L_chr, dynamic_var_nm_1L_chr, 
    dynamic_var_rsl_1L_chr, featured_var_pfx_1L_chr = "", group_by_var_1L_chr, 
    reference_var_rsl_1L_chr, reference_vals_chr) 
{
    profiled_sf <- update_popl_by_incld_area(profiled_sf = profiled_sf, 
        spatial_unit_1L_chr = dynamic_var_rsl_1L_chr, data_year_1L_chr = data_year_1L_chr, 
        concept_1L_chr = reference_vals_chr[2], featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
    if (featured_var_pfx_1L_chr == "") 
        profiled_sf <- update_popl_by_group(profiled_sf = profiled_sf, 
            group_by_var_1L_chr = group_by_var_1L_chr, dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr, 
            data_year_1L_chr = data_year_1L_chr, dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
            reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, 
            featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, 
            reference_vals_chr = reference_vals_chr)
    return(profiled_sf)
}
