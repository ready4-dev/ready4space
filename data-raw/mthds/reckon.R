reckon.vicinity_mapes <- function(x,
                                  iter_1L_int = 1,
                                  n_its_int = 1,
                                  joint_dstr_1L_lgl = T,
                                  type_1L_chr = "outer"){
  if(type_1L_chr == "inner"){
    if(joint_dstr_1L_lgl){
      params_vals_tb <- x_vicinity_mapes %>%
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
      params_vals_tb <- x_vicinity_parameters %>%
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
    mape_xx <- params_vals_tb %>%
      dplyr::select(var_nm_chr,#sex_age_band_chr,
                    mape_05_yr,mape_10_yr,mape_15_yr) %>%
      tidyr::gather(key = "param_name_chr",
                    value = !!paste0("v_it_",iter_1L_int),
                    mape_05_yr,
                    mape_10_yr,
                    mape_15_yr) %>%
      tidyr::unite(param_name_chr,
                   var_nm_chr,#sex_age_band_chr,
                   sep="_",
                   col="param_name_chr")
  }
  if(type_1L_chr == "outer"){
    if(n_its_int==1){
      mape_xx <- calculate_mape(x_vicinity_parameters = x,
                                iter_1L_int = 1,
                                joint_dstr_1L_lgl = joint_dstr_1L_lgl,
                                var_nm_1L_chr = var_nm_1L_chr)
    }else{
      reduce_list <- purrr::prepend(2:n_its_int,
                                    list(calculate_mape(x_vicinity_parameters = x,
                                                        iter_1L_int = 1,
                                                        joint_dstr_1L_lgl = joint_dstr_1L_lgl)))
      mape_xx <- purrr::reduce(reduce_list,
                               ~ dplyr::inner_join(.x,
                                                   calculate_mape(x_vicinity_parameters = x,
                                                                  iter_1L_int = .y,
                                                                  joint_dstr_1L_lgl = joint_dstr_1L_lgl))) # %>%   rfwn_param_val_mape()
    }
    }

  return(mape_xx)
}
reckon.vicinity_parameters <- function(x,
                                       iter_1L_int = integer(0),
                                       n_its_int = integer(0),
                                       type_1L_chr = "outer"){
  if(type_1L_chr == "inner"){ #calculate_val_from_dstr
    val_xx <- x %>%
      dplyr::mutate(!!paste0("v_it_",iter_1L_int) := transform_multiple_vals(distribution_chr,
                                                                             dstr_param_1_dbl,
                                                                             dstr_param_2_dbl,
                                                                             dstr_param_3_dbl,
                                                                             transformation_chr))

  }
  if(type_1L_chr == "outer"){
    if(n_its_int==1){
      val_xx <- reckon(x, #calculate_val_from_dstr
                       iter_1L_int = 1,
                       type_1L_chr = "inner")
    }else{
      reduce_ls <- purrr::prepend(2:n_its_int,
                                    list(reckon(x, #calculate_val_from_dstr
                                                iter_1L_int = 1,
                                                type_1L_chr = "inner")))
      val_xx <- purrr::reduce(reduce_ls,
                              ~ dplyr::inner_join(.x,
                                                  reckon(x,#calculate_val_from_dstr
                                                         iter_1L_int = .y,
                                                         type_1L_chr = "inner"))) %>%
        dplyr::select(-c(distribution_chr,
                         dstr_param_1_dbl,
                         dstr_param_2_dbl,
                         dstr_param_3_dbl,
                         transformation_chr)) #%>%  rfwn_param_val_envir()
    }
  }
  return(val_xx)
}
