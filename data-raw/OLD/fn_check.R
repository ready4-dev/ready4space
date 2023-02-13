#' Check if ppr
#' @description validate_popl_predns_incld() is a Check function that performs a validity check. Specifically, this function implements an algorithm to check if ppr. The function is called for its side effects and does not return a value.
#' @param data_name_item PARAM_DESCRIPTION
#' @param data_lookup_tb Data lookup (a tibble)
#' @param popl_predns_var_1L_chr PARAM_DESCRIPTION
#' @return NULL
#' @rdname validate_popl_predns_incld
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4fun get_from_lup
#' @importFrom stringr str_detect
#' @importFrom magrittr is_greater_than
validate_popl_predns_incld <- function (data_name_item, data_lookup_tb, popl_predns_var_1L_chr) 
{
    purrr::map_chr(data_name_item, ~ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb, 
        match_value_xx = .x, match_var_nm_1L_chr = "name", target_var_nm_1L_chr = "main_feature", 
        evaluate_1L_lgl = FALSE)) %>% stringr::str_detect(popl_predns_var_1L_chr) %>% 
        sum() %>% magrittr::is_greater_than(0)
}
