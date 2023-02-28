remove_grouped_popl_vars <- function(profiled_sf,
                                     featured_var_pfx_1L_chr){
  var_names_chr <- profiled_sf %>% names()
  keep_vars_chr <- var_names_chr[!var_names_chr  %>% startsWith("whl_") & !var_names_chr  %>% startsWith("grp_by_") & !var_names_chr  %>% startsWith("dupl_")]
  keep_vars_chr <- keep_vars_chr[!keep_vars_chr %>% startsWith("inc_") | keep_vars_chr %>% startsWith(featured_var_pfx_1L_chr)]
  profiled_sf <- dplyr::select(profiled_sf,
                               keep_vars_chr)
  return(profiled_sf)

}
remove_outlier_areas <- function(areas_sf,# suburb_based_effective_catch_excl_outliers
                                 outliers_chr,
                                 area_var_nm_1L_chr = character(0)){#SSC_NAME16
  included_areas_with_outliers_chr <- areas_sf %>%
    dplyr::pull(!!rlang::sym(area_var_nm_1L_chr))
  remove_outlier <- function(x,y) x[!stringr::str_detect(x,y)]
  included_areas_excl_outliers_chr <- as.list(outliers_chr) %>%
    stats::setNames(outliers_chr) %>%
    purrr::prepend(list(a = included_areas_with_outliers_chr)) %>%
    purrr::reduce(remove_outlier)
  areas_sf <- areas_sf %>%
    dplyr::filter(!!rlang::sym(area_var_nm_1L_chr) %in%
                    included_areas_excl_outliers_chr) %>%
    sf::st_as_sf()
  return(areas_sf)
}
