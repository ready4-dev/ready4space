check_if_ppr <- function(data_name_item,
                         data_lookup_tb,
                         pop_projs_str){
  purrr::map_chr(data_name_item,
                 ~ ready4fun::get_from_lup(data_lookup_tb = data_lookup_tb,
                                        lookup_reference = .x,
                                        lookup_variable = "name_chr",
                                        target_variable = "main_feature_chr",
                                        evaluate = FALSE)) %>%
    stringr::str_detect(pop_projs_str) %>%
    sum() %>%
    magrittr::is_greater_than(0)
}

