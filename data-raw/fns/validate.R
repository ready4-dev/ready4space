validate_popl_predns_incld <- function(data_items_chr,
                         data_lookup_tb,
                         key_var_1L_chr,#pop_projs_str
                         ){
  contains_popl_predns_1L_lgl <- purrr::map_chr(data_items_chr,
                    ~ ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
                                               match_value_xx = .x,
                                               match_var_nm_1L_chr = "name_chr",
                                               target_var_nm_1L_chr = "main_feature_chr",
                                               evaluate_1L_lgl = FALSE)) %>%
                           stringr::str_detect(key_var_1L_chr) %>%
                           sum() %>%
                           magrittr::is_greater_than(0)
  return(contains_popl_predns_1L_lgl)
}

