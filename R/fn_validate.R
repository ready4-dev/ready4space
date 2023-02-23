#' Validate population preductions included
#' @description validate_popl_predns_incld() is a Validate function that validates that an object conforms to required criteria. Specifically, this function implements an algorithm to validate population preductions included. The function returns Contains population preductions (a logical vector of length one).
#' @param data_items_chr Data items (a character vector)
#' @param data_lookup_tb Data lookup (a tibble)
#' @param key_var_1L_chr Key variable (a character vector of length one)
#' @return Contains population preductions (a logical vector of length one)
#' @rdname validate_popl_predns_incld
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4 get_from_lup_obj
#' @importFrom stringr str_detect
#' @importFrom magrittr is_greater_than
#' @keywords internal
validate_popl_predns_incld <- function (data_items_chr, data_lookup_tb, key_var_1L_chr) 
{
    contains_popl_predns_1L_lgl <- purrr::map_chr(data_items_chr, 
        ~ready4::get_from_lup_obj(data_lookup_tb = data_lookup_tb, 
            match_value_xx = .x, match_var_nm_1L_chr = "name_chr", 
            target_var_nm_1L_chr = "main_feature_chr", evaluate_1L_lgl = FALSE)) %>% 
        stringr::str_detect(key_var_1L_chr) %>% sum() %>% magrittr::is_greater_than(0)
    return(contains_popl_predns_1L_lgl)
}
