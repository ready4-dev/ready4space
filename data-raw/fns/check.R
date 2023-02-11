check_if_ppr <- function(data_name_item,
                         data_lookup_tb,
                         pop_projs_str){
  purrr::map_chr(data_name_item,
                 ~ ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb,
                                        match_value_xx = .x,
                                        match_var_nm_1L_chr = "name_chr",
                                        target_var_nm_1L_chr = "main_feature_chr",
                                        evaluate_1L_lgl = FALSE)) %>%
    stringr::str_detect(pop_projs_str) %>%
    sum() %>%
    magrittr::is_greater_than(0)
}

