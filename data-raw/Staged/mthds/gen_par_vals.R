gen_param_vals.ready4_mapes <- function(x,
                                                  n_its_int,
                                                  jt_dist){
  if(n_its_int==1){
    gen_val_mape(params_struc_tb = x,
                 it_nbr = 1,
                 jt_dist = jt_dist)
  }else{
    reduce_list <- purrr::prepend(2:n_its_int,
                                  list(gen_val_mape(params_struc_tb = x,
                                                    it_nbr = 1,
                                                    jt_dist = jt_dist)))
    purrr::reduce(reduce_list,
                  ~ dplyr::inner_join(.x,
                                      gen_val_mape(params_struc_tb = x,
                                                   it_nbr = .y,
                                                   jt_dist = jt_dist))) %>%
      rfwn_param_val_mape()
  }
}
gen_param_vals.vicinity_parameters <- function(x,
                                              n_its_int){
  if(n_its_int==1){
    gen_val_envir(params_struc_tb = x,
                  it_nbr = 1)
  }else{
    reduce_list <- purrr::prepend(2:n_its_int,
                                  list(gen_val_envir(params_struc_tb = x,
                                                     it_nbr = 1)))
    purrr::reduce(reduce_list,
                  ~ dplyr::inner_join(.x,
                                      gen_val_envir(params_struc_tb = x,
                                                    it_nbr = .y))) %>%
      dplyr::select(-c(distribution_chr,
                       dstr_param_1_dbl,
                       dstr_param_2_dbl,
                       dstr_param_3_dbl,
                       transformation_chr)) %>%
      rfwn_param_val_envir()
  }
}
