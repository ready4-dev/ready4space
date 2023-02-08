subset_sf_by_feature <- function(profiled_sf,
                                 group_by_var){
  purrr::map(profiled_sf %>%
               dplyr::pull(!!rlang::sym(group_by_var)) %>%
               unique(),
             ~ profiled_sf %>%
               dplyr::filter(!!rlang::sym(group_by_var) == .x)) %>%
    stats::setNames(profiled_sf %>%
                      dplyr::pull(!!rlang::sym(group_by_var)) %>%
                      unique())
}
subset_sf_ls_by_common_vars <- function(sf_ls){
  common_vars_vec <- get_common_vars_sf_ls(sf_ls)
  purrr::map(sf_ls, ~ .x %>% dplyr::select(common_vars_vec))

}
