update_pop_count_by_areas <-function(profiled_sf,
                                     group_by_var,
                                     age_sex_var_name,
                                     data_year_chr,
                                     age_sex_pop_resolution,
                                     tot_pop_resolution,
                                     popl_var_prefix = ""){
  profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                        sp_unit = age_sex_pop_resolution,
                                        data_year_chr = data_year_chr,
                                        concept = "age_sex",
                                        popl_var_prefix = popl_var_prefix)
  if(popl_var_prefix == "")
    profiled_sf <- sum_pop_by_multiple_groups_sf(profiled_sf = profiled_sf,
                                                 group_by_var = group_by_var,
                                                 age_sex_var_name = age_sex_var_name,
                                                 data_year_chr = data_year_chr,
                                                 age_sex_pop_resolution = age_sex_pop_resolution,
                                                 tot_pop_resolution = tot_pop_resolution,
                                                 popl_var_prefix = popl_var_prefix)

  return(profiled_sf)
}
update_pop_by_inc_area <- function(profiled_sf,
                                   sp_unit,
                                   data_year_chr,
                                   concept,
                                   age_sex_var_name = NULL,
                                   age_sex_pop_resolution = NULL,
                                   tot_pop_col = NULL,
                                   popl_var_prefix){
  nse_objs_ls <- make_nse_objs_ls(sp_unit = sp_unit,
                                          concept = concept,
                                          tot_pop_col = tot_pop_col,
                                          grouping_1 = age_sex_pop_resolution,
                                          data_year_chr = data_year_chr,
                                          popl_var_prefix = popl_var_prefix)
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$area_inc_unit) := sf::st_area(.) %>%
                    units::set_units(km^2))
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$prop_inc_unit) := as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit)))
  if(!is.null(tot_pop_col)){
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(!!rlang::sym(nse_objs_ls$popl_inc_unit) := !!rlang::sym(nse_objs_ls$popl_whl_unit) * !!rlang::sym(nse_objs_ls$prop_inc_unit))
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          nse_objs_ls = nse_objs_ls,
                                          grp_var_name = age_sex_var_name)
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(pop_prop_multiplier_tot_pop = !!rlang::sym(nse_objs_ls$popl_inc_unit) / !!rlang::sym(nse_objs_ls$grouping_1_concept_tot))  %>%
      dplyr::mutate(pop_prop_multiplier_tot_pop = ifelse(is.nan(pop_prop_multiplier_tot_pop),
                                                         0,
                                                         pop_prop_multiplier_tot_pop))
  }
  profiled_sf <- profiled_sf %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_1),
                                 dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_2)),
                     dplyr::funs(!!rlang::sym(nse_objs_ls$popl_inc_unit) := .*!!rlang::sym(nse_objs_ls$popl_multiplier))) %>%
    transform_sfx_to_pfx(suffix = nse_objs_ls$popl_inc_unit) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                     dplyr::funs(gsub(nse_objs_ls$inc_str_to_delete,
                                      "",
                                      .)))
  return(profiled_sf)
}

update_sf_boundary_descr <- function(look_up_ref,
                                     one_cluster_up_to_xmin_list){
  max_var <- "max"
  max_vec <- one_cluster_up_to_xmin_list %>%
    purrr::pluck(look_up_ref) %>%
    names() %>%
    stringr::str_subset("max")
  if(length(max_vec) > 1){
    max_var <- max_vec %>%
      stringr::str_subset("max.") %>%
      stringr::str_replace_all("max.","") %>%
      as.numeric() %>%
      max() %>%
      paste0("max.",.)
  }
  return_object <- one_cluster_up_to_xmin_list %>%
    purrr::pluck(look_up_ref) %>%
    dplyr::mutate(max := !!rlang::sym(max_var)) %>%
    dplyr::mutate(center = (min + max) / 2) %>%
    dplyr::mutate(drive_times = paste0("0 to ",max," mins")) %>%
    dplyr::select(id,min,max,center,drive_times)
  return(return_object)
}
update_sp_data_list <- function(sp_data_list,
                                input_ls,
                                profiled_area_bands_list){
  crs_nbr_dbl <-  input_ls$pa_r4 %>% crs_nbr()
  at_highest_res = input_ls$at_highest_res
  distance_km = geom_dist_limit_km(input_ls$pa_r4)
  travel_time_mins = drive_time_limit_mins(input_ls$pa_r4)
  group_by_var <- get_group_by_var_from_pai(input_ls$pa_r4)
  age_sex_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_ls$age_sex_pop_str) + 1]
  age_sex_counts_grouped_by <- ready4fun::get_from_lup(data_lookup_tb = lookup_tb(input_ls$pa_r4) %>%
                                                       sp_uid_lup() %>%
                                                       dplyr::filter(year_chr %in% c(input_ls$pa_r4@data_year_chr)),
                                                     lookup_variable = "spatial_unit_chr",
                                                     lookup_reference = age_sex_pop_resolution,
                                                     target_variable = "var_name_chr",
                                                     evaluate = FALSE)
  tot_pop_resolution <- NULL
  if(!is.null(input_ls$tot_pop_str)){
    tot_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_ls$tot_pop_str) + 1]
    res_lup <- input_ls$pa_r4 %>% lookup_tb() %>% sp_resolution_lup()
    use_tot_pop_lgl <- c(age_sex_pop_resolution,tot_pop_resolution) %>%
      purrr::map_dbl(~ready4fun::get_from_lup(data_lookup_tb = res_lup,
                                            lookup_variable = "area_type_chr",
                                            lookup_reference = .x,
                                            target_variable = "mean_size_dbl",
                                            evaluate = F)) %>%
      nnet::which.is.max() == 1
    if(!use_tot_pop_lgl)
      tot_pop_resolution <- NULL

  }
  # if(use_coord_lup(input_ls$pa_r4))
  #   profiled_area_bands_list <- purrr::map(profiled_area_bands_list,
  #                                          ~ .x %>%
  #                                            sf::st_transform(crs_nbr(input_ls$pa_r4)[1]))
  by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list,
                                         ~ intersect_sfs_update_counts(profiled_sf = .x,
                                                                       profiled_colref = NA,
                                                                       profiled_rowref = NA,
                                                                       sp_data_list = sp_data_list,
                                                                       tot_pop_resolution = tot_pop_resolution,
                                                                       age_sex_pop_resolution = age_sex_pop_resolution,
                                                                       group_by_var = group_by_var,
                                                                       age_sex_counts_grouped_by = age_sex_counts_grouped_by,
                                                                       data_year_chr = input_ls$pa_r4@data_year_chr,
                                                                       crs_nbr_dbl = crs_nbr_dbl))
  by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls,
                                          names(by_band_pop_counts_sf_ls),
                                          ~ .x %>%
                                            dplyr::mutate(pop_sp_unit_id = paste0(.y,
                                                                                  "_",
                                                                                  tolower(age_sex_pop_resolution),
                                                                                  "_",
                                                                                  rownames(.x))) %>%
                                            dplyr::mutate(pop_sp_unit_area = sf::st_area(.)))
  profiled_sf <- do.call(rbind,by_band_pop_counts_sf_ls)
  popl_var_prefix <- get_popl_var_prefix(age_sex_pop_resolution = age_sex_pop_resolution,
                                         tot_pop_resolution = tot_pop_resolution,
                                         data_year_chr = input_ls$pa_r4@data_year_chr)
  profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf,
                                        popl_var_prefix = popl_var_prefix)
  profiled_sf <- add_dynamic_sp_vars_to_sf(dynamic_sp_vars_sf = sp_data_list[[sp_data_list$ppr_ref[1]]] %>%
                                             dplyr::select(1),
                                           pop_attr_sf = profiled_sf,
                                           age_sex_pop_resolution = "UNIT_ID",
                                           age_sex_var_name = "pop_sp_unit_id",
                                           popl_var_prefix = popl_var_prefix,
                                           data_year_chr = input_ls$pa_r4@data_year_chr,
                                           crs_nbr_dbl = crs_nbr_dbl)
  extended_sp_data_list <- append(sp_data_list,
                                  list(profiled_sf = profiled_sf,
                                       popl_var_prefix = popl_var_prefix)) # Is pop_val_prefix needed in this list?
  return(extended_sp_data_list)
}
update_spProcessed_r4 <- function(x) {
  lookup_tbs_r4 <- x@lup_tbs_r4
  sp_import_lup <- lookup_tbs_r4@sp_import_lup
  ready4use::assert_single_row_tb(sp_import_lup)
  if(sp_import_lup$data_type_chr == "Geometry"){
    lookup_tbs_r4 <- add_starter_sf_to_lups(lookup_tbs_r4,
                                            path_to_seed_sf_1L_chr = x@path_to_seed_sf_1L_chr) %>%
      add_uid_lup()
  }
  #}
  lookup_tbs_r4 %>%
    add_data_pack_lup(template_ls = x@imports_ls,
                      tb_data_type = sp_import_lup$data_type_chr,
                      pckg_name = x@pkg_1L_chr)

}


