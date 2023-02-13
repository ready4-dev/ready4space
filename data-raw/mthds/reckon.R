reckon.vicinity_mapes <- function(x,
                                  n_its_int,
                                  joint_dstr_1L_lgl){
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
  return(mape_xx)
}
reckon.vicinity_parameters <- function(x,
                                       n_its_int){
  if(n_its_int==1){
    val_xx <- calculate_val_from_dstr(x_vicinity_parameters = x,
                            iter_1L_int = 1)
  }else{
    reduce_list <- purrr::prepend(2:n_its_int,
                                  list(calculate_val_from_dstr(x_vicinity_parameters = x,
                                                               iter_1L_int = 1)))
    val_xx <- purrr::reduce(reduce_list,
                            ~ dplyr::inner_join(.x,
                                                calculate_val_from_dstr(x_vicinity_parameters = x,
                                                                        iter_1L_int = .y))) %>%
      dplyr::select(-c(distribution_chr,
                       dstr_param_1_dbl,
                       dstr_param_2_dbl,
                       dstr_param_3_dbl,
                       transformation_chr)) #%>%  rfwn_param_val_envir()
  }
  return(val_xx)
}
