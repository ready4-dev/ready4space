#' Check if ppr
#' @description check_if_ppr() is a Check function that performs a validity check. Specifically, this function implements an algorithm to check if ppr. The function is called for its side effects and does not return a value.
#' @param data_name_item PARAM_DESCRIPTION
#' @param data_lookup_tb Data lookup (a tibble)
#' @param pop_projs_str PARAM_DESCRIPTION
#' @return NULL
#' @rdname check_if_ppr
#' @export 
#' @importFrom purrr map_chr
#' @importFrom ready4fun get_from_lup
#' @importFrom stringr str_detect
#' @importFrom magrittr is_greater_than
#' @keywords internal
check_if_ppr <- function (data_name_item, data_lookup_tb, pop_projs_str) 
{
    purrr::map_chr(data_name_item, ~ready4fun::get_from_lup(data_lookup_tb = data_lookup_tb, 
        lookup_reference = .x, lookup_variable = "name", target_variable = "main_feature", 
        evaluate = FALSE)) %>% stringr::str_detect(pop_projs_str) %>% 
        sum() %>% magrittr::is_greater_than(0)
}
