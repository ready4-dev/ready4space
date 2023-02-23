update_isochrone_tbl <- function(index_val_1L_int,
                                 temporal_bands_ls,
                                 travel_mode_1L_chr){
  max_var_1L_chr <- "isomax"
  max_vars_chr <- temporal_bands_ls %>%
    purrr::pluck(index_val_1L_int) %>%
    names() %>%
    stringr::str_subset("isomax")
  if(length(max_vars_chr) > 1){
    max_var_1L_chr <- max_vars_chr %>%
      stringr::str_subset("isomax.") %>% # CHECK
      stringr::str_replace_all("isomax.","") %>%
      as.numeric() %>%
      max() %>%
      paste0("isomax.",.)
  }
  isochrone_tb <- temporal_bands_ls %>%
    purrr::pluck(index_val_1L_int) %>%
    dplyr::mutate(isomax := !!rlang::sym(max_var_1L_chr)) %>% # max
    dplyr::mutate(center_value = (isomin + isomax) / 2) %>% #center
    dplyr::mutate(!!rlang::sym(paste0(travel_mode_1L_chr,"_times")) := paste0("0 to ",isomax," mins")) %>% #drive_times
    dplyr::select(id,isomin,isomax,center_value,!!rlang::sym(paste0(travel_mode_1L_chr,"_times")) )
  return(isochrone_tb)
}
update_popl_by_group <- function(profiled_sf, # sum_pop_by_multiple_groups_sf
                                 data_year_1L_chr,
                                 dynamic_var_nm_1L_chr,
                                 dynamic_var_rsl_1L_chr,
                                 group_by_var_1L_chr,
                                 reference_var_rsl_1L_chr,
                                 featured_var_pfx_1L_chr = "",
                                 reference_vals_chr #= c("tot_pop","age_sex")
                                 ){
  profiled_sf <- add_popl_counts(profiled_sf = profiled_sf,
                                 group_by_var_1L_chr = dynamic_var_nm_1L_chr,
                                 nse_objs_ls = make_nse_objs_ls(spatial_unit_1L_chr = dynamic_var_rsl_1L_chr,
                                                                concept_1L_chr = reference_vals_chr[2],#"age_sex",
                                                                grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                                                data_year_1L_chr = data_year_1L_chr,
                                                                featured_var_pfx_1L_chr = featured_var_pfx_1L_chr),
                                 convert_sfx_to_pfx_1L_lgl = TRUE)
  if(!is.null(reference_var_rsl_1L_chr)){
    profiled_sf <- update_popl_by_incld_area(profiled_sf = profiled_sf,
                                             spatial_unit_1L_chr = reference_var_rsl_1L_chr,
                                             data_year_1L_chr = data_year_1L_chr,
                                             concept_1L_chr = reference_vals_chr[1],#"tot_pop",
                                             reference_var_nm_1L_chr = paste0("year_",
                                                                  data_year_1L_chr,
                                                                  "pr"),
                                             dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr,
                                             dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr)
  }
  if(!is.null(reference_var_rsl_1L_chr)){
    profiled_sf <- add_popl_counts(profiled_sf = profiled_sf,
                                   group_by_var_1L_chr = dynamic_var_nm_1L_chr,
                                   nse_objs_ls = make_nse_objs_ls(spatial_unit_1L_chr = reference_var_rsl_1L_chr,
                                                                  concept_1L_chr = reference_vals_chr[1],#"tot_pop",
                                                                  reference_var_nm_1L_chr = paste0("year_",
                                                                                       data_year_1L_chr,
                                                                                       "pr"),
                                                                  grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                                                  data_year_1L_chr = data_year_1L_chr),
                                   convert_sfx_to_pfx_1L_lgl = TRUE)
  }
  profiled_sf <- add_popl_counts(profiled_sf = profiled_sf,
                                 group_by_var_1L_chr = group_by_var_1L_chr,
                                 nse_objs_ls = make_nse_objs_ls(concept_1L_chr = reference_vals_chr[2],#"age_sex",
                                                                spatial_unit_1L_chr = dynamic_var_rsl_1L_chr,
                                                                grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                                                data_year_1L_chr = data_year_1L_chr,
                                                                featured_var_pfx_1L_chr = featured_var_pfx_1L_chr),
                                 convert_sfx_to_pfx_1L_lgl = TRUE,
                                 top_level_1L_lgl = TRUE)
  if(!is.null(reference_var_rsl_1L_chr)){
    profiled_sf <- add_popl_counts(profiled_sf = profiled_sf,
                                   group_by_var_1L_chr = group_by_var_1L_chr,
                                   nse_objs_ls = make_nse_objs_ls(spatial_unit_1L_chr = reference_var_rsl_1L_chr,
                                                                  concept_1L_chr = reference_vals_chr[1],#"tot_pop",
                                                                  reference_var_nm_1L_chr = paste0("year_",
                                                                                       data_year_1L_chr,
                                                                                       "pr"),
                                                                  grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                                                  data_year_1L_chr = data_year_1L_chr),
                                   convert_sfx_to_pfx_1L_lgl = TRUE,
                                   top_level_1L_lgl = TRUE)
  }
  return(profiled_sf)
}
update_popl_counts <- function(profiled_sf,
                               data_year_1L_chr,
                               dynamic_var_nm_1L_chr,
                               dynamic_var_rsl_1L_chr,
                               featured_var_pfx_1L_chr = "",
                               group_by_var_1L_chr,
                               reference_var_rsl_1L_chr,
                               reference_vals_chr #= c("tot_pop","age_sex")
){
  profiled_sf <- update_popl_by_incld_area(profiled_sf = profiled_sf,
                                           spatial_unit_1L_chr = dynamic_var_rsl_1L_chr,
                                           data_year_1L_chr = data_year_1L_chr,
                                           concept_1L_chr = reference_vals_chr[2],#"age_sex",
                                           featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
  if(featured_var_pfx_1L_chr == "")
    profiled_sf <- update_popl_by_group(profiled_sf = profiled_sf,
                                        group_by_var_1L_chr = group_by_var_1L_chr,
                                        dynamic_var_nm_1L_chr = dynamic_var_nm_1L_chr,
                                        data_year_1L_chr = data_year_1L_chr,
                                        dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
                                        reference_var_rsl_1L_chr = reference_var_rsl_1L_chr,
                                        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
                                        reference_vals_chr = reference_vals_chr)

  return(profiled_sf)
}
update_popl_by_incld_area <- function(profiled_sf,
                                      concept_1L_chr,
                                      data_year_1L_chr,
                                      featured_var_pfx_1L_chr,
                                      spatial_unit_1L_chr,
                                      dynamic_var_nm_1L_chr = NULL,
                                      dynamic_var_rsl_1L_chr = NULL,
                                      reference_var_nm_1L_chr = NULL
                                      ){
  nse_objs_ls <- make_nse_objs_ls(spatial_unit_1L_chr = spatial_unit_1L_chr,
                                          concept_1L_chr = concept_1L_chr,
                                          reference_var_nm_1L_chr = reference_var_nm_1L_chr,
                                          grouping_var_1L_chr = dynamic_var_rsl_1L_chr,
                                          data_year_1L_chr = data_year_1L_chr,
                                          featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$area_inc_unit) := sf::st_area(.) %>%
                    units::set_units(km^2))
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$prop_inc_unit) := as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit)))
  if(!is.null(reference_var_nm_1L_chr)){
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(!!rlang::sym(nse_objs_ls$popl_inc_unit) := !!rlang::sym(nse_objs_ls$popl_whl_unit) * !!rlang::sym(nse_objs_ls$prop_inc_unit))
    profiled_sf <- add_popl_counts(profiled_sf = profiled_sf,
                                          nse_objs_ls = nse_objs_ls,
                                          group_by_var_1L_chr = dynamic_var_nm_1L_chr)
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
    transform_sfx_to_pfx(suffix_1L_chr = nse_objs_ls$popl_inc_unit) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                     dplyr::funs(gsub(nse_objs_ls$inc_str_to_delete,
                                      "",
                                      .)))
  return(profiled_sf)
}
# sum_at_diff_funs <- function(data_sf,
#                              var_list,
#                              funs_list,
#                              group_by){
#   ## https://github.com/tidyverse/dplyr/issues/3101
#   purrr::map2(var_list,
#               funs_list,
#               ~ data_sf %>%
#                 dplyr::group_by(!!rlang::sym(group_by)) %>%
#                 dplyr::summarise_at(.x, .y)) %>%
#     #purrr::reduce(dplyr::inner_join)
#     purrr::reduce(sf::st_join) %>%
#     dplyr::select(-dplyr::one_of(paste0(group_by,".y"))) %>%
#     dplyr::rename(!!rlang::sym(group_by) := paste0(group_by,".x"))
# }
# update_spatial_atts_ls <- function(spatial_atts_ls, # Probably formerly extend_sp_data_ls or else combination of extend_sp_data_ls and reformatting from https://github.com/orygennp/ready4sim/blob/master/R/fn_make_sim_data_env.R
#                                     input_ls, #Now manufacture mthd
#                                     profiled_area_bands_ls){
#   crs_nbr_dbl <-  input_ls$x_VicinityProfile %>% crs_nbr()
#   at_highest_res = input_ls$at_highest_res
#   distance_in_km_dbl = geom_dist_limit_km(input_ls$x_VicinityProfile)
#   travel_time_mins = drive_time_limit_mins(input_ls$x_VicinityProfile)
#   group_by_var_1L_chr <- procure(input_ls$x_VicinityProfile,#get_group_by_var_from_VicinityProfile
#                                  what_1L_chr = "grouping")
#   dynamic_var_rsl_1L_chr <- names(spatial_atts_ls)[which(at_highest_res == input_ls$age_sex_pop_str) + 1]
#   reference_grouping_1L_chr <- ready4::get_from_lup_obj(data_lookup_tb = lookup_tb(input_ls$x_VicinityProfile) %>%
#                                                           sp_uid_lup() %>%
#                                                           dplyr::filter(year_chr %in% c(input_ls$x_VicinityProfile@data_year_1L_chr)),
#                                                         match_var_nm_1L_chr = "spatial_unit_chr",
#                                                         match_value_xx = dynamic_var_rsl_1L_chr,
#                                                         target_var_nm_1L_chr = "var_name_chr",
#                                                         evaluate_1L_lgl = FALSE)
#   reference_var_rsl_1L_chr <- NULL
#   if(!is.null(input_ls$tot_pop_str)){
#     reference_var_rsl_1L_chr <- names(spatial_atts_ls)[which(at_highest_res == input_ls$tot_pop_str) + 1]
#     res_lup <- input_ls$x_VicinityProfile %>% lookup_tb() %>% sp_resolution_lup()
#     use_tot_pop_lgl <- c(dynamic_var_rsl_1L_chr,reference_var_rsl_1L_chr) %>%
#       purrr::map_dbl(~ready4::get_from_lup_obj(data_lookup_tb = res_lup,
#                                                match_var_nm_1L_chr = "area_type_chr",
#                                                match_value_xx = .x,
#                                                target_var_nm_1L_chr = "mean_size_dbl",
#                                                evaluate_1L_lgl = F)) %>%
#       nnet::which.is.max() == 1
#     if(!use_tot_pop_lgl)
#       reference_var_rsl_1L_chr <- NULL
#
#   }
#   # if(use_coord_lup(input_ls$x_VicinityProfile))
#   #   profiled_area_bands_ls <- purrr::map(profiled_area_bands_ls,
#   #                                          ~ .x %>%
#   #                                            sf::st_transform(crs_nbr(input_ls$x_VicinityProfile)[1]))
#   by_band_pop_counts_sf_ls <- purrr::map(profiled_area_bands_ls,
#                                          ~ make_reconciled_intersecting_area(profiled_sf = .x,
#                                                                              profiled_sf_col_1L_chr = NA,
#                                                                              profiled_sf_row_1L_chr = NA,
#                                                                              spatial_atts_ls = spatial_atts_ls,
#                                                                              reference_var_rsl_1L_chr = reference_var_rsl_1L_chr,
#                                                                              dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
#                                                                              group_by_var_1L_chr = group_by_var_1L_chr,
#                                                                              reference_grouping_1L_chr = reference_grouping_1L_chr,
#                                                                              data_year_1L_chr = input_ls$x_VicinityProfile@data_year_1L_chr,
#                                                                              crs_nbr_dbl = crs_nbr_dbl))
#   by_band_pop_counts_sf_ls <- purrr::map2(by_band_pop_counts_sf_ls,
#                                           names(by_band_pop_counts_sf_ls),
#                                           ~ .x %>%
#                                             dplyr::mutate(popl_spatial_unit_chr = paste0(.y,
#                                                                                          "_",
#                                                                                          tolower(dynamic_var_rsl_1L_chr),
#                                                                                          "_",
#                                                                                          rownames(.x))) %>%
#                                             dplyr::mutate(popl_spatial_unit_area_dbl = sf::st_area(.)))
#   profiled_sf <- do.call(rbind,by_band_pop_counts_sf_ls)
#   featured_var_pfx_1L_chr <- make_featured_var_pfx(dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr,
#                                                    reference_var_rsl_1L_chr = reference_var_rsl_1L_chr,
#                                                    data_year_1L_chr = input_ls$x_VicinityProfile@data_year_1L_chr)
#   profiled_sf <- remove_grouped_popl_vars(profiled_sf = profiled_sf,
#                                           featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)
#   profiled_sf <- add_dynamic_vars_to_sf(dynamic_vars_sf = spatial_atts_ls[[spatial_atts_ls$ppr_idx_dbl[1]]] %>%
#                                           dplyr::select(1),
#                                         profiled_sf = profiled_sf,
#                                         dynamic_var_rsl_1L_chr = "UNIT_ID",
#                                         dynamic_var_nm_1L_chr = "popl_spatial_unit_chr",
#                                         featured_var_pfx_1L_chr = featured_var_pfx_1L_chr,
#                                         data_year_1L_chr = input_ls$x_VicinityProfile@data_year_1L_chr,
#                                         crs_nbr_dbl = crs_nbr_dbl)
#   extended_spatial_atts_ls <- append(spatial_atts_ls,
#                                       list(profiled_sf = profiled_sf,
#                                            featured_var_pfx_1L_chr = featured_var_pfx_1L_chr)) # Is pop_val_prefix needed in this list?
#   return(extended_spatial_atts_ls)
# }
