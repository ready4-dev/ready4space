sum_at_diff_funs <- function(data_sf,
                             var_list,
                             funs_list,
                             group_by){
  ## https://github.com/tidyverse/dplyr/issues/3101
  purrr::map2(var_list,
              funs_list,
              ~ data_sf %>%
                dplyr::group_by(!!rlang::sym(group_by)) %>%
                dplyr::summarise_at(.x, .y)) %>%
    #purrr::reduce(dplyr::inner_join)
    purrr::reduce(sf::st_join) %>%
    dplyr::select(-dplyr::one_of(paste0(group_by,".y"))) %>%
    dplyr::rename(!!rlang::sym(group_by) := paste0(group_by,".x"))
}
sum_pop_by_multiple_groups_sf <- function(profiled_sf,
                                          group_by_var,
                                          age_sex_var_name,
                                          data_year,
                                          age_sex_pop_resolution,
                                          tot_pop_resolution,
                                          popl_var_prefix = ""){
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                        grp_var_name = age_sex_var_name,
                                        nse_objs_ls = make_nse_objs_ls(sp_unit = age_sex_pop_resolution,
                                                                       concept = "age_sex",
                                                                       grouping_1 = age_sex_pop_resolution,
                                                                       data_year = data_year,
                                                                       popl_var_prefix = popl_var_prefix),
                                        suff_to_pref = TRUE)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                          sp_unit = tot_pop_resolution,
                                          data_year = data_year,
                                          concept = "tot_pop",
                                          tot_pop_col = paste0("year_",
                                                               data_year,
                                                               "pr"),
                                          age_sex_var_name = age_sex_var_name,
                                          age_sex_pop_resolution = age_sex_pop_resolution)
  }
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = age_sex_var_name,
                                          nse_objs_ls = make_nse_objs_ls(sp_unit = tot_pop_resolution,
                                                                         concept = "tot_pop",
                                                                         tot_pop_col = paste0("year_",
                                                                                              data_year,
                                                                                              "pr"),
                                                                         grouping_1 = age_sex_pop_resolution,
                                                                         data_year = data_year),
                                          suff_to_pref = TRUE)
  }
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                        grp_var_name = group_by_var,
                                        nse_objs_ls = make_nse_objs_ls(age_sex_pop_resolution,
                                                                       concept = "age_sex",
                                                                       grouping_1 = age_sex_pop_resolution,
                                                                       data_year = data_year,
                                                                       popl_var_prefix = popl_var_prefix),
                                        suff_to_pref = TRUE,
                                        top_level = TRUE)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = group_by_var,
                                          nse_objs_ls = make_nse_objs_ls(sp_unit = tot_pop_resolution,
                                                                         concept = "tot_pop",
                                                                         tot_pop_col = paste0("year_",
                                                                                              data_year,
                                                                                              "pr"),
                                                                         grouping_1 = age_sex_pop_resolution,
                                                                         data_year = data_year),
                                          suff_to_pref = TRUE,
                                          top_level = TRUE)
  }
}


sum_updated_pop_by_grp <- function(profiled_sf,
                                   grp_var_name,
                                   nse_objs_ls,
                                   suff_to_pref = FALSE,
                                   top_level = FALSE){
  group_totals <- profiled_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(!!rlang::sym(grp_var_name)) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                        dplyr::funs(!!rlang::sym(nse_objs_ls$grouping_1_concept_tot) := sum(.)))
  if(suff_to_pref)
    group_totals <- group_totals %>%
      transform_sfx_to_pfx(suffix = nse_objs_ls$grouping_1_concept_tot)
  group_totals <- group_totals %>%
    dplyr::ungroup()
  if(top_level){
    dplyr::bind_cols(profiled_sf,
                     group_totals[rep(row.names(group_totals), nrow(profiled_sf)), ] %>%
                       dplyr::select(-!!rlang::sym(grp_var_name)))
  }else{
    profiled_sf %>%
      dplyr::inner_join(.,
                        group_totals %>%
                          dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$grouping_1_concept_tot)),
                                           dplyr::funs(gsub(paste0(nse_objs_ls$popl_inc_unit,"_"),"",.))))
  }
}
