remove_grouped_popl_vars <- function(profiled_sf,
                                   popl_var_prefix){
  var_names_vec <- profiled_sf %>% names()
  keep_vars_vec <- var_names_vec[!var_names_vec  %>% startsWith("whl_") & !var_names_vec  %>% startsWith("grp_by_") & !var_names_vec  %>% startsWith("dupl_")]
  keep_vars_vec <- keep_vars_vec[!keep_vars_vec %>% startsWith("inc_") | keep_vars_vec %>% startsWith(popl_var_prefix)]
  dplyr::select(profiled_sf,
                keep_vars_vec)

}
