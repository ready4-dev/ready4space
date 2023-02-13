#' Subset simple features object by feature
#' @description make_sf_ls() is a Subset function that subsets and object. Specifically, this function implements an algorithm to subset simple features object by feature. The function is called for its side effects and does not return a value.
#' @param profiled_sf Profiled (a simple features object)
#' @param group_by_var_1L_chr PARAM_DESCRIPTION
#' @return NULL
#' @rdname make_sf_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom stats setNames
make_sf_ls <- function (profiled_sf, group_by_var_1L_chr) 
{
    purrr::map(profiled_sf %>% dplyr::pull(!!rlang::sym(group_by_var_1L_chr)) %>% 
        unique(), ~profiled_sf %>% dplyr::filter(!!rlang::sym(group_by_var_1L_chr) == 
        .x)) %>% stats::setNames(profiled_sf %>% dplyr::pull(!!rlang::sym(group_by_var_1L_chr)) %>% 
        unique())
}
#' Subset simple features object list by common vars
#' @description transform_sf_ls() is a Subset function that subsets and object. Specifically, this function implements an algorithm to subset simple features object list by common vars. The function is called for its side effects and does not return a value.
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return NULL
#' @rdname transform_sf_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr select
transform_sf_ls <- function (sf_ls) 
{
    common_vars_vec <- make_common_sf_vars_ls(sf_ls)
    purrr::map(sf_ls, ~.x %>% dplyr::select(common_vars_vec))
}
