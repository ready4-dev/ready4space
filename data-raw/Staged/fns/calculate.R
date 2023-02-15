# calculate_val_from_dstr <- function(x_vicinity_parameters, #
#                                     iter_1L_int){
#   val_num <- x_vicinity_parameters %>%
#     dplyr::mutate(!!paste0("v_it_",iter_1L_int) := transform_multiple_vals(distribution_chr,
#                                                                            dstr_param_1_dbl,
#                                                                            dstr_param_2_dbl,
#                                                                            dstr_param_3_dbl,
#                                                                            transformation_chr))
#   return(val_num)
# }
# calculate_mape <- function(x_vicinity_mapes,
#                            iter_1L_int,
#                            joint_dstr_1L_lgl){
#   if(joint_dstr_1L_lgl){
#     params_vals_tb <- x_vicinity_mapes %>%
#       dplyr::mutate(jt_dstr_loc_2 = runif(1)) %>%
#       dplyr::mutate(mape_05_yr = mc2d::qpert(p = jt_dstr_loc_2,
#                                              mode = mape_05_yr_mde_dbl,
#                                              min = mape_05_yr_min_dbl,
#                                              max = mape_05_yr_max_dbl,
#                                              shape = mape_05_yr_shp_dbl),
#                     mape_10_yr = mc2d::qpert(p = jt_dstr_loc_2,
#                                              mode = mape_10_yr_mde_dbl,
#                                              min = mape_10_yr_min_dbl,
#                                              max = mape_10_yr_max_dbl,
#                                              shape = mape_10_yr_shp_dbl),
#                     mape_15_yr = mc2d::qpert(p = jt_dstr_loc_2,
#                                              mode = mape_15_yr_mde_dbl,
#                                              min = mape_15_yr_min_dbl,
#                                              max = mape_15_yr_max_dbl,
#                                              shape = mape_15_yr_shp_dbl))
#   }else{
#     params_vals_tb <- x_vicinity_parameters %>%
#       dplyr::mutate(nbr_draws = 1) %>%
#       dplyr::mutate(mape_05_yr = mc2d::rpert(n = nbr_draws,
#                                              mode = mape_05_yr_mde_dbl,
#                                              min = mape_05_yr_min_dbl,
#                                              max = mape_05_yr_max_dbl,
#                                              shape = mape_05_yr_shp_dbl),
#                     mape_10_yr = mc2d::rpert(n = nbr_draws,
#                                              mode = mape_10_yr_mde_dbl,
#                                              min = mape_10_yr_min_dbl,
#                                              max = mape_10_yr_max_dbl,
#                                              shape = mape_10_yr_shp_dbl),
#                     mape_15_yr = mc2d::rpert(n = nbr_draws,
#                                              mode = mape_15_yr_mde_dbl,
#                                              min = mape_15_yr_min_dbl,
#                                              max = mape_15_yr_max_dbl,
#                                              shape = mape_15_yr_shp_dbl))
#   }
#   mapes_tb <- params_vals_tb %>%
#     dplyr::select(var_nm_chr,#sex_age_band_chr,
#                   mape_05_yr,mape_10_yr,mape_15_yr) %>%
#     tidyr::gather(key = "param_name_chr",
#                   value = !!paste0("v_it_",iter_1L_int),
#                   mape_05_yr,
#                   mape_10_yr,
#                   mape_15_yr) %>%
#     tidyr::unite(param_name_chr,
#                  var_nm_chr,#sex_age_band_chr,
#                  sep="_",
#                  col="param_name_chr")
#   return(mapes_tb)
# }
