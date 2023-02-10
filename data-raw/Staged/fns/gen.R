gen_val_envir <- function(params_struc_tb,
                          it_nbr){
  params_struc_tb %>%
    dplyr::mutate(!!paste0("v_it_",it_nbr) := gen_val_envir_vect(distribution_chr,
                                                                 dstr_param_1_dbl,
                                                                 dstr_param_2_dbl,
                                                                 dstr_param_3_dbl,
                                                                 transformation_chr))
}
gen_val_envir_sngl <- function(distribution_1L_chr, # IS THIS CORRECT? LOOKS LIKE A GET TFMN FN
                              dstr_param_1_1L_dbl,
                              dstr_param_2_1L_dbl,
                              dstr_param_3_1L_dbl,
                              transformation_1L_chr){
  if(distribution_chr == "none")
    x <- dstr_param_1_1L_dbl
  if(!is.na(transformation_1L_chr))
    x <- eval(parse(text=transformation_1L_chr))
  return(x)
}
gen_val_envir_vect <- function(distribution_chr,
                               dstr_param_1_dbl,
                               dstr_param_2_dbl,
                               dstr_param_3_dbl,
                               transformation_chr){
  purrr::map_dbl(1:length(distribution_chr),
                 ~ gen_val_envir_sngl(distribution_chr[.x],
                                     dstr_param_1_dbl[.x],
                                     dstr_param_2_dbl[.x],
                                     dstr_param_3_dbl[.x],
                                     transformation_chr[.x]))
}
gen_val_mape <- function(params_struc_tb,
                         it_nbr,
                         jt_dist){
  if(jt_dist){
    params_vals_tb <- params_struc_tb %>%
      dplyr::mutate(jt_dstr_loc_2 = runif(1)) %>%
      dplyr::mutate(mape_05_yr = mc2d::qpert(p = jt_dstr_loc_2,
                                             mode = mape_05_yr_mde_dbl,
                                             min = mape_05_yr_min_dbl,
                                             max = mape_05_yr_max_dbl,
                                             shape = mape_05_yr_shp_dbl),
                    mape_10_yr = mc2d::qpert(p = jt_dstr_loc_2,
                                             mode = mape_10_yr_mde_dbl,
                                             min = mape_10_yr_min_dbl,
                                             max = mape_10_yr_max_dbl,
                                             shape = mape_10_yr_shp_dbl),
                    mape_15_yr = mc2d::qpert(p = jt_dstr_loc_2,
                                             mode = mape_15_yr_mde_dbl,
                                             min = mape_15_yr_min_dbl,
                                             max = mape_15_yr_max_dbl,
                                             shape = mape_15_yr_shp_dbl))
  }else{
    params_vals_tb <- params_struc_tb %>%
      dplyr::mutate(nbr_draws = 1) %>%
      dplyr::mutate(mape_05_yr = mc2d::rpert(n = nbr_draws,
                                             mode = mape_05_yr_mde_dbl,
                                             min = mape_05_yr_min_dbl,
                                             max = mape_05_yr_max_dbl,
                                             shape = mape_05_yr_shp_dbl),
                    mape_10_yr = mc2d::rpert(n = nbr_draws,
                                             mode = mape_10_yr_mde_dbl,
                                             min = mape_10_yr_min_dbl,
                                             max = mape_10_yr_max_dbl,
                                             shape = mape_10_yr_shp_dbl),
                    mape_15_yr = mc2d::rpert(n = nbr_draws,
                                             mode = mape_15_yr_mde_dbl,
                                             min = mape_15_yr_min_dbl,
                                             max = mape_15_yr_max_dbl,
                                             shape = mape_15_yr_shp_dbl))
  }
  params_vals_tb %>%
    dplyr::select(sex_age_band_chr,mape_05_yr,mape_10_yr,mape_15_yr) %>%
    tidyr::gather(key = "param_name_chr",
                  value = !!paste0("v_it_",it_nbr),
                  mape_05_yr,
                  mape_10_yr,
                  mape_15_yr) %>%
    tidyr::unite(param_name_chr,
                 sex_age_band_chr,
                 sep="_",
                 col="param_name_chr")
}
