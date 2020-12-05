rename_vars_based_on_res <- function(sf,
                                     feature_nm,
                                     data_type,
                                     data_year,
                                     popl_var_prefix){
  feature_res_chr_vec <- get_res_specific_vars(var_names = names(sf),
                                               data_type = data_type,
                                               data_year = data_year,
                                               popl_var_prefix = popl_var_prefix)
  sf %>%
    dplyr::rename_at(dplyr::vars(feature_res_chr_vec),
                     dplyr::funs(paste0("whl_",
                                        feature_nm,
                                        "_",
                                        .)))
}
