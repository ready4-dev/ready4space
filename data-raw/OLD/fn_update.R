#' Update pop by include area
#' @description update_pop_by_inc_area() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update pop by include area. Function argument profiled_sf specifies the object to be updated. Argument sp_unit provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param sp_unit PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param concept PARAM_DESCRIPTION
#' @param dynamic_var_nm_1L_chr PARAM_DESCRIPTION, Default: NULL
#' @param dynamic_var_rsl_1L_chr PARAM_DESCRIPTION, Default: NULL
#' @param reference_var_nm_1L_chr PARAM_DESCRIPTION, Default: NULL
#' @param featured_var_pfx_1L_chr PARAM_DESCRIPTION
#' @return Profiled (a simple features object)
#' @rdname update_pop_by_inc_area
#' @export
#' @importFrom dplyr mutate mutate_at vars starts_with funs rename_at
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
update_pop_by_inc_area <- function (profiled_sf, sp_unit, data_year, concept, dynamic_var_nm_1L_chr = NULL,
    dynamic_var_rsl_1L_chr = NULL, reference_var_nm_1L_chr = NULL, featured_var_pfx_1L_chr)
{
    nse_objs_ls <- make_nse_objs_ls(sp_unit = sp_unit, concept = concept,
        reference_var_nm_1L_chr = reference_var_nm_1L_chr, grouping_1 = dynamic_var_rsl_1L_chr,
        data_year = data_year, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$area_inc_unit),
        sf::st_area(.) %>% units::set_units(km^2)))
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$prop_inc_unit),
        as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit))))
    if (!is.null(reference_var_nm_1L_chr)) {
        profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$popl_inc_unit),
            !!rlang::sym(nse_objs_ls$popl_whl_unit) * !!rlang::sym(nse_objs_ls$prop_inc_unit)))
        profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
            nse_objs_ls = nse_objs_ls, grp_var_name = dynamic_var_nm_1L_chr)
        profiled_sf <- profiled_sf %>% dplyr::mutate(pop_prop_multiplier_tot_pop = !!rlang::sym(nse_objs_ls$popl_inc_unit)/!!rlang::sym(nse_objs_ls$grouping_1_concept_tot)) %>%
            dplyr::mutate(pop_prop_multiplier_tot_pop = ifelse(is.nan(pop_prop_multiplier_tot_pop),
                0, pop_prop_multiplier_tot_pop))
    }
    profiled_sf <- profiled_sf %>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_1),
        dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_2)),
        dplyr::funs(`:=`(!!rlang::sym(nse_objs_ls$popl_inc_unit),
            . * !!rlang::sym(nse_objs_ls$popl_multiplier)))) %>%
        transform_sfx_to_pfx(suffix = nse_objs_ls$popl_inc_unit) %>%
        dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
            dplyr::funs(gsub(nse_objs_ls$inc_str_to_delete, "",
                .)))
    return(profiled_sf)
}
#' Update pop count by areas
#' @description update_pop_count_by_areas() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update pop count by areas. Function argument profiled_sf specifies the object to be updated. Argument group_by_var_1L_chr provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param group_by_var_1L_chr PARAM_DESCRIPTION
#' @param dynamic_var_nm_1L_chr PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param dynamic_var_rsl_1L_chr PARAM_DESCRIPTION
#' @param reference_var_rsl_1L_chr PARAM_DESCRIPTION
#' @param featured_var_pfx_1L_chr PARAM_DESCRIPTION, Default: ''
#' @return Profiled (a simple features object)
#' @rdname update_pop_count_by_areas
#' @export

update_pop_count_by_areas <- function (profiled_sf, group_by_var_1L_chr, dynamic_var_nm_1L_chr, data_year,
    dynamic_var_rsl_1L_chr, reference_var_rsl_1L_chr, featured_var_pfx_1L_chr = "")
{
    profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
        sp_unit = dynamic_var_rsl_1L_chr, data_year = data_year,
        concept = "age_sex", featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
    if (featured_var_pfx_1L_chr == "")
        profiled_sf <- sum_pop_by_multiple_groups_sf(profiled_sf = profiled_sf,
            group_by_var_1L_chr = group_by_var_1L_chr, dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr,
            data_year = data_year, dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
            reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
    return(profiled_sf)
}
#' Update simple features object boundary descr
#' @description update_sf_boundary_descr() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update simple features object boundary descr. Function argument look_up_ref specifies the object to be updated. Argument one_cluster_up_to_xmin_list provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param look_up_ref PARAM_DESCRIPTION
#' @param one_cluster_up_to_xmin_list PARAM_DESCRIPTION
#' @return NA ()
#' @rdname update_sf_boundary_descr
#' @export
#' @importFrom purrr pluck
#' @importFrom stringr str_subset str_replace_all
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
update_sf_boundary_descr <- function (look_up_ref, one_cluster_up_to_xmin_list)
{
    max_var <- "max"
    max_vec <- one_cluster_up_to_xmin_list %>% purrr::pluck(look_up_ref) %>%
        names() %>% stringr::str_subset("max")
    if (length(max_vec) > 1) {
        max_var <- max_vec %>% stringr::str_subset("max.") %>%
            stringr::str_replace_all("max.", "") %>% as.numeric() %>%
            max() %>% paste0("max.", .)
    }
    return_object <- one_cluster_up_to_xmin_list %>% purrr::pluck(look_up_ref) %>%
        dplyr::mutate(`:=`(max, !!rlang::sym(max_var))) %>% dplyr::mutate(center = (min +
        max)/2) %>% dplyr::mutate(drive_times = paste0("0 to ",
        max, " mins")) %>% dplyr::select(id, min, max, center,
        drive_times)
    return(return_object)
}
#' Update sp data list
#' @description update_sp_data_list() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update sp data list. Function argument sp_data_list specifies the object to be updated. Argument input_ls provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param sp_data_list PARAM_DESCRIPTION
#' @param input_ls Input (a list)
#' @param profiled_area_bands_list PARAM_DESCRIPTION
#' @return NA ()
#' @rdname update_sp_data_list
#' @export
#' @importFrom ready4fun get_from_lup
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_dbl map map2
#' @importFrom nnet which.is.max
#' @importFrom sf st_area
update_sp_data_list <- function (sp_data_list, input_ls, profiled_area_bands_list)
{
    crs_nbr_dbl <- input_ls$x_VicinityProfile %>% crs_nbr()
    at_highest_res = input_ls$at_highest_res
    distance_km = geom_dist_limit_km(input_ls$x_VicinityProfile)
    travel_time_mins = drive_time_limit_mins(input_ls$x_VicinityProfile)
    group_by_var_1L_chr <- get_group_by_var_from_VicinityProfile(input_ls$x_VicinityProfile)
    dynamic_var_rsl_1L_chr <- names(sp_data_list)[which(at_highest_res ==
        input_ls$age_sex_pop_str) + 1]
    age_sex_counts_grouped_by <- ready4::get_from_lup_obj(data_lookup_tb = lookup_tb(input_ls$x_VicinityProfile) %>%
        sp_uid_lup() %>% dplyr::filter(year %in% c(data_year(input_ls$x_VicinityProfile))),
        match_var_nm_1L_chr = "spatial_unit", match_value_xx = dynamic_var_rsl_1L_chr,
        target_var_nm_1L_chr = "var_name", evaluate_1L_lgl = FALSE)
    reference_var_rsl_1L_chr <- NULL
    if (!is.null(input_ls$tot_pop_str)) {
        reference_var_rsl_1L_chr <- names(sp_data_list)[which(at_highest_res ==
            input_ls$tot_pop_str) + 1]
        res_lup <- input_ls$x_VicinityProfile %>% lookup_tb() %>% sp_resolution_lup()
        use_tot_pop_lgl <- c(dynamic_var_rsl_1L_chr, reference_var_rsl_1L_chr) %>%
            purrr::map_dbl(~ready4::get_from_lup_obj(data_lookup_tb = res_lup,
                match_var_nm_1L_chr = "area_type", match_value_xx = .x,
                target_var_nm_1L_chr = "mean_size", evaluate_1L_lgl = F)) %>%
            nnet::which.is.max() == 1
        if (!use_tot_pop_lgl)
            reference_var_rsl_1L_chr <- NULL
    }
    by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list,
        ~make_reconciled_intersecting_area(profiled_sf = .x, profiled_sf_col_1L_chr = NA,
            profiled_sf_row_1L_chr = NA, sp_data_list = sp_data_list,
            reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
            group_by_var_1L_chr = group_by_var_1L_chr, age_sex_counts_grouped_by = age_sex_counts_grouped_by,
            data_year = data_year(input_ls$x_VicinityProfile), crs_nbr_dbl = crs_nbr_dbl))
    by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls,
        names(by_band_pop_counts_sf_ls), ~.x %>% dplyr::mutate(popl_spatial_unit_chr = paste0(.y,
            "_", tolower(dynamic_var_rsl_1L_chr), "_", rownames(.x))) %>%
            dplyr::mutate(popl_spatial_unit_area_dbl = sf::st_area(.)))
    profiled_sf <- do.call(rbind, by_band_pop_counts_sf_ls)
    featured_var_pfx_1L_chr <- make_featured_var_pfx(dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
        reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, data_year = data_year(input_ls$x_VicinityProfile))
    profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf,
        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
    profiled_sf <- add_dynamic_vars_to_sf(dynamic_vars_sf = sp_data_list[[sp_data_list$ppr_ref[1]]] %>%
        dplyr::select(1), profiled_sf = profiled_sf, dynamic_var_rsl_1L_chr = "UNIT_ID",
        dynamic_var_nm_1L_chr = "popl_spatial_unit_chr", featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
        data_year = input_ls$x_VicinityProfile@data_year, crs_nbr_dbl = crs_nbr_dbl)
    extended_sp_data_list <- append(sp_data_list, list(profiled_sf = profiled_sf,
        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr))
    return(extended_sp_data_list)
}
#' Update sp local process
#' @description update_spProcessed_r4() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update sp local process ready4 s4. Function argument x specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_spProcessed_r4
#' @export
#' @importFrom ready4use assert_single_row_tb
update_spProcessed_r4 <- function (x)
{
    y_VicinityLookup <- x@lup_tbs_r4
    z_vicinity_raw <- y_VicinityLookup@vicinity_raw_r3#sp_import_lup
    ready4use::assert_single_row_tb(z_vicinity_raw)
    if (z_vicinity_raw$data_type == "Geometry") {
        y_VicinityLookup <- add_templates(y_VicinityLookup,
            path_to_seed_sf_1L_chr = x@path_to_seed_sf_1L_chr) %>%
            add_uid_lup()
    }
    y_VicinityLookup <- y_VicinityLookup %>% renew(template_ls = x@imports_ls,#add_data_pack_lup
                               tbl_data_type_1L_chr = z_vicinity_raw$data_type,
                               package_1L_chr = x@pkg_1L_chr,
                               what_1L_chr = "processed")
    return(y_VicinityLookup)
}
