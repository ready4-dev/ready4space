#' Update pop by include area
#' @description update_pop_by_inc_area() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update pop by include area. Function argument profiled_sf specifies the object to be updated. Argument sp_unit provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param sp_unit PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param concept PARAM_DESCRIPTION
#' @param age_sex_var_name PARAM_DESCRIPTION, Default: NULL
#' @param age_sex_pop_resolution PARAM_DESCRIPTION, Default: NULL
#' @param tot_pop_col PARAM_DESCRIPTION, Default: NULL
#' @param popl_var_prefix PARAM_DESCRIPTION
#' @return Profiled (a simple features object)
#' @rdname update_pop_by_inc_area
#' @export 
#' @importFrom dplyr mutate mutate_at vars starts_with funs rename_at
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
update_pop_by_inc_area <- function (profiled_sf, sp_unit, data_year, concept, age_sex_var_name = NULL, 
    age_sex_pop_resolution = NULL, tot_pop_col = NULL, popl_var_prefix) 
{
    nse_objs_ls <- make_nse_objs_ls(sp_unit = sp_unit, concept = concept, 
        tot_pop_col = tot_pop_col, grouping_1 = age_sex_pop_resolution, 
        data_year = data_year, popl_var_prefix = popl_var_prefix)
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$area_inc_unit), 
        sf::st_area(.) %>% units::set_units(km^2)))
    profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$prop_inc_unit), 
        as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit))))
    if (!is.null(tot_pop_col)) {
        profiled_sf <- profiled_sf %>% dplyr::mutate(`:=`(!!rlang::sym(nse_objs_ls$popl_inc_unit), 
            !!rlang::sym(nse_objs_ls$popl_whl_unit) * !!rlang::sym(nse_objs_ls$prop_inc_unit)))
        profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf, 
            nse_objs_ls = nse_objs_ls, grp_var_name = age_sex_var_name)
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
#' @description update_pop_count_by_areas() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update pop count by areas. Function argument profiled_sf specifies the object to be updated. Argument group_by_var provides the object to be updated. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param group_by_var PARAM_DESCRIPTION
#' @param age_sex_var_name PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION, Default: ''
#' @return Profiled (a simple features object)
#' @rdname update_pop_count_by_areas
#' @export 

update_pop_count_by_areas <- function (profiled_sf, group_by_var, age_sex_var_name, data_year, 
    age_sex_pop_resolution, tot_pop_resolution, popl_var_prefix = "") 
{
    profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf, 
        sp_unit = age_sex_pop_resolution, data_year = data_year, 
        concept = "age_sex", popl_var_prefix = popl_var_prefix)
    if (popl_var_prefix == "") 
        profiled_sf <- sum_pop_by_multiple_groups_sf(profiled_sf = profiled_sf, 
            group_by_var = group_by_var, age_sex_var_name = age_sex_var_name, 
            data_year = data_year, age_sex_pop_resolution = age_sex_pop_resolution, 
            tot_pop_resolution = tot_pop_resolution, popl_var_prefix = popl_var_prefix)
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
    crs_nbr_vec <- input_ls$pa_r4 %>% crs_nbr()
    at_highest_res = input_ls$at_highest_res
    distance_km = geom_dist_limit_km(input_ls$pa_r4)
    travel_time_mins = drive_time_limit_mins(input_ls$pa_r4)
    group_by_var <- get_group_by_var_from_pai(input_ls$pa_r4)
    age_sex_pop_resolution <- names(sp_data_list)[which(at_highest_res == 
        input_ls$age_sex_pop_str) + 1]
    age_sex_counts_grouped_by <- ready4fun::get_from_lup(data_lookup_tb = lookup_tb(input_ls$pa_r4) %>% 
        sp_uid_lup() %>% dplyr::filter(year %in% c(data_year(input_ls$pa_r4))), 
        lookup_variable = "spatial_unit", lookup_reference = age_sex_pop_resolution, 
        target_variable = "var_name", evaluate = FALSE)
    tot_pop_resolution <- NULL
    if (!is.null(input_ls$tot_pop_str)) {
        tot_pop_resolution <- names(sp_data_list)[which(at_highest_res == 
            input_ls$tot_pop_str) + 1]
        res_lup <- input_ls$pa_r4 %>% lookup_tb() %>% sp_resolution_lup()
        use_tot_pop_lgl <- c(age_sex_pop_resolution, tot_pop_resolution) %>% 
            purrr::map_dbl(~ready4fun::get_from_lup(data_lookup_tb = res_lup, 
                lookup_variable = "area_type", lookup_reference = .x, 
                target_variable = "mean_size", evaluate = F)) %>% 
            nnet::which.is.max() == 1
        if (!use_tot_pop_lgl) 
            tot_pop_resolution <- NULL
    }
    by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list, 
        ~intersect_sfs_update_counts(profiled_sf = .x, profiled_colref = NA, 
            profiled_rowref = NA, sp_data_list = sp_data_list, 
            tot_pop_resolution = tot_pop_resolution, age_sex_pop_resolution = age_sex_pop_resolution, 
            group_by_var = group_by_var, age_sex_counts_grouped_by = age_sex_counts_grouped_by, 
            data_year = data_year(input_ls$pa_r4), crs_nbr_vec = crs_nbr_vec))
    by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls, 
        names(by_band_pop_counts_sf_ls), ~.x %>% dplyr::mutate(pop_sp_unit_id = paste0(.y, 
            "_", tolower(age_sex_pop_resolution), "_", rownames(.x))) %>% 
            dplyr::mutate(pop_sp_unit_area = sf::st_area(.)))
    profiled_sf <- do.call(rbind, by_band_pop_counts_sf_ls)
    popl_var_prefix <- get_popl_var_prefix(age_sex_pop_resolution = age_sex_pop_resolution, 
        tot_pop_resolution = tot_pop_resolution, data_year = data_year(input_ls$pa_r4))
    profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf, 
        popl_var_prefix = popl_var_prefix)
    profiled_sf <- add_dynamic_sp_vars_to_sf(dynamic_sp_vars_sf = sp_data_list[[sp_data_list$ppr_ref[1]]] %>% 
        dplyr::select(1), pop_attr_sf = profiled_sf, age_sex_pop_resolution = "UNIT_ID", 
        age_sex_var_name = "pop_sp_unit_id", popl_var_prefix = popl_var_prefix, 
        data_year = input_ls$pa_r4@data_year, crs_nbr_vec = crs_nbr_vec)
    extended_sp_data_list <- append(sp_data_list, list(profiled_sf = profiled_sf, 
        popl_var_prefix = popl_var_prefix))
    return(extended_sp_data_list)
}
#' Update sp local process
#' @description update_sp_local_proc_r4() is an Update function that edits an object, while preserving core object attributes. Specifically, this function implements an algorithm to update sp local process ready4 s4. Function argument x specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname update_sp_local_proc_r4
#' @export 
#' @importFrom ready4use assert_single_row_tb
update_sp_local_proc_r4 <- function (x) 
{
    lookup_tbs_r4 <- x@lup_tbs_r4
    sp_import_lup <- lookup_tbs_r4@sp_import_lup
    ready4use::assert_single_row_tb(sp_import_lup)
    if (sp_import_lup$data_type == "Geometry") {
        lookup_tbs_r4 <- add_starter_sf_to_lups(lookup_tbs_r4, 
            path_to_starter_sf_chr = x@path_to_starter_sf_chr) %>% 
            add_uid_lup()
    }
    lookup_tbs_r4 %>% add_data_pack_lup(template_ls = x@import_this_ls, 
        tb_data_type = sp_import_lup$data_type, pckg_name = x@pckg_chr)
}
