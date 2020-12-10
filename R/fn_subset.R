#' Subset simple features object by feature
#' @description subset_sf_by_feature() is a Subset function that subsets and object. Specifically, this function implements an algorithm to subset simple features object by feature. The function is called for its side effects and does not return a value.
#' @param profiled_sf Profiled (a simple features object)
#' @param group_by_var PARAM_DESCRIPTION
#' @return NULL
#' @rdname subset_sf_by_feature
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @keywords internal
subset_sf_by_feature <- function (profiled_sf, group_by_var) 
{
    purrr::map(profiled_sf %>% dplyr::pull(!!rlang::sym(group_by_var)) %>% 
        unique(), ~profiled_sf %>% dplyr::filter(!!rlang::sym(group_by_var) == 
        .x)) %>% stats::setNames(profiled_sf %>% dplyr::pull(!!rlang::sym(group_by_var)) %>% 
        unique())
}
#' Subset simple features object list by common vars
#' @description subset_sf_ls_by_common_vars() is a Subset function that subsets and object. Specifically, this function implements an algorithm to subset simple features object list by common vars. The function is called for its side effects and does not return a value.
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return NULL
#' @rdname subset_sf_ls_by_common_vars
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr select
#' @keywords internal
subset_sf_ls_by_common_vars <- function (sf_ls) 
{
    common_vars_vec <- get_common_vars_sf_ls(sf_ls)
    purrr::map(sf_ls, ~.x %>% dplyr::select(common_vars_vec))
}
