update_pop_count_by_areas <-function(profiled_sf,
                                     group_by_var_1L_chr,
                                     dynamic_var_nm_1L_chr,
                                     data_year_chr,
                                     dynamic_var_rsl_1L_chr,
                                     tot_pop_resolution,
                                     featured_var_pfx_1L_chr = ""){
  profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                        spatial_unit_1L_chr = dynamic_var_rsl_1L_chr,
                                        data_year_chr = data_year_chr,
                                        concept = "age_sex",
                                        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
  if(featured_var_pfx_1L_chr == "")
    profiled_sf <- sum_pop_by_multiple_groups_sf(profiled_sf = profiled_sf,
                                                 group_by_var_1L_chr = group_by_var_1L_chr,
                                                 dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr,
                                                 data_year_chr = data_year_chr,
                                                 dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                                 tot_pop_resolution = tot_pop_resolution,
                                                 featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)

  return(profiled_sf)
}
update_pop_by_inc_area <- function(profiled_sf,
                                   spatial_unit_1L_chr,
                                   data_year_chr,
                                   concept,
                                   dynamic_var_nm_1L_chr = NULL,
                                   dynamic_var_rsl_1L_chr = NULL,
                                   tot_pop_col = NULL,
                                   featured_var_pfx_1L_chr){
  nse_objs_ls <- make_nse_objs_ls(spatial_unit_1L_chr = spatial_unit_1L_chr,
                                          concept = concept,
                                          tot_pop_col = tot_pop_col,
                                          grouping_1 = dynamic_var_rsl_1L_chr,
                                          data_year_chr = data_year_chr,
                                          featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
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
                                          grp_var_name = dynamic_var_nm_1L_chr)
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
  crs_nbr_dbl <-  input_ls$x_VicinityProfile %>% crs_nbr()
  at_highest_res = input_ls$at_highest_res
  distance_km = geom_dist_limit_km(input_ls$x_VicinityProfile)
  travel_time_mins = drive_time_limit_mins(input_ls$x_VicinityProfile)
  group_by_var_1L_chr <- procure(input_ls$x_VicinityProfile,#get_group_by_var_from_VicinityProfile
                                 what_1L_chr = "grouping")
  dynamic_var_rsl_1L_chr <- names(sp_data_list)[which(at_highest_res == input_ls$age_sex_pop_str) + 1]
  age_sex_counts_grouped_by <- ready4::get_from_lup_obj(data_lookup_tb = lookup_tb(input_ls$x_VicinityProfile) %>%
                                                       sp_uid_lup() %>%
                                                       dplyr::filter(year_chr %in% c(input_ls$x_VicinityProfile@data_year_chr)),
                                                     match_var_nm_1L_chr = "spatial_unit_chr",
                                                     match_value_xx = dynamic_var_rsl_1L_chr,
                                                     target_var_nm_1L_chr = "var_name_chr",
                                                     evaluate_1L_lgl = FALSE)
  tot_pop_resolution <- NULL
  if(!is.null(input_ls$tot_pop_str)){
    tot_pop_resolution <- names(sp_data_list)[which(at_highest_res == input_ls$tot_pop_str) + 1]
    res_lup <- input_ls$x_VicinityProfile %>% lookup_tb() %>% sp_resolution_lup()
    use_tot_pop_lgl <- c(dynamic_var_rsl_1L_chr,tot_pop_resolution) %>%
      purrr::map_dbl(~ready4::get_from_lup_obj(data_lookup_tb = res_lup,
                                            match_var_nm_1L_chr = "area_type_chr",
                                            match_value_xx = .x,
                                            target_var_nm_1L_chr = "mean_size_dbl",
                                            evaluate_1L_lgl = F)) %>%
      nnet::which.is.max() == 1
    if(!use_tot_pop_lgl)
      tot_pop_resolution <- NULL

  }
  # if(use_coord_lup(input_ls$x_VicinityProfile))
  #   profiled_area_bands_list <- purrr::map(profiled_area_bands_list,
  #                                          ~ .x %>%
  #                                            sf::st_transform(crs_nbr(input_ls$x_VicinityProfile)[1]))
  by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_list,
                                         ~ make_reconciled_intersecting_area(profiled_sf = .x,
                                                                       profiled_sf_col_1L_chr = NA,
                                                                       profiled_sf_row_1L_chr = NA,
                                                                       sp_data_list = sp_data_list,
                                                                       tot_pop_resolution = tot_pop_resolution,
                                                                       dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                                                       group_by_var_1L_chr = group_by_var_1L_chr,
                                                                       age_sex_counts_grouped_by = age_sex_counts_grouped_by,
                                                                       data_year_chr = input_ls$x_VicinityProfile@data_year_chr,
                                                                       crs_nbr_dbl = crs_nbr_dbl))
  by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls,
                                          names(by_band_pop_counts_sf_ls),
                                          ~ .x %>%
                                            dplyr::mutate(popl_spatial_unit_chr = paste0(.y,
                                                                                  "_",
                                                                                  tolower(dynamic_var_rsl_1L_chr),
                                                                                  "_",
                                                                                  rownames(.x))) %>%
                                            dplyr::mutate(popl_spatial_unit_area_dbl = sf::st_area(.)))
  profiled_sf <- do.call(rbind,by_band_pop_counts_sf_ls)
  featured_var_pfx_1L_chr <- make_featured_var_pfx(dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                         tot_pop_resolution = tot_pop_resolution,
                                         data_year_chr = input_ls$x_VicinityProfile@data_year_chr)
  profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf,
                                        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
  profiled_sf <- add_dynamic_vars_to_sf(dynamic_vars_sf = sp_data_list[[sp_data_list$ppr_ref[1]]] %>%
                                             dplyr::select(1),
                                           profiled_sf = profiled_sf,
                                           dynamic_var_rsl_1L_chr = "UNIT_ID",
                                           dynamic_var_nm_1L_chr = "popl_spatial_unit_chr",
                                           featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
                                           data_year_chr = input_ls$x_VicinityProfile@data_year_chr,
                                           crs_nbr_dbl = crs_nbr_dbl)
  extended_sp_data_list <- append(sp_data_list,
                                  list(profiled_sf = profiled_sf,
                                       featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)) # Is pop_val_prefix needed in this list?
  return(extended_sp_data_list)
}



