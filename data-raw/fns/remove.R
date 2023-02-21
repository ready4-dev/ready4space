remove_grouped_popl_vars <- function(profiled_sf,
                                     featured_var_pfx_1L_chr){
  var_names_chr <- profiled_sf %>% names()
  keep_vars_chr <- var_names_chr[!var_names_chr  %>% startsWith("whl_") & !var_names_chr  %>% startsWith("grp_by_") & !var_names_chr  %>% startsWith("dupl_")]
  keep_vars_chr <- keep_vars_chr[!keep_vars_chr %>% startsWith("inc_") | keep_vars_chr %>% startsWith(featured_var_pfx_1L_chr)]
  profiled_sf <- dplyr::select(profiled_sf,
                               keep_vars_chr)
  return(profiled_sf)

}
