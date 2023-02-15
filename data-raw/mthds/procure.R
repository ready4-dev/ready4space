procure.vicinity_abbreviations <- function(x,
                                           col_nm_1L_chr = "short_name_chr",
                                           match_value_xx){
 object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                       match_value_xx = match_value_xx,
                                       match_var_nm_1L_chr = col_nm_1L_chr,
                                       target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "short_name_chr","long_name_chr","short_name_chr"),
                                       evaluate_1L_lgl = FALSE)
 return(object_xx)
}
procure.vicinity_identifiers <- function(x,
                                         col_nm_1L_chr = "spatial_unit_chr",
                                         geometry_rsl_1L_chr = character(0),
                                         group_at_geom_unit_1L_lgl = TRUE,
                                         data_rsl_1L_chr = character(0),
                                         match_value_xx = NULL,
                                         area_bndy_yr_chr = character(0),
                                         what_1L_chr = "match"){
  if(what_1L_chr == "match"){
    object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x %>% dplyr::filter(year_chr == area_bndy_yr_chr),
                                          match_value_xx = match_value_xx,
                                          match_var_nm_1L_chr = col_nm_1L_chr,
                                          target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "spatial_unit_chr","var_name_chr","spatial_unit_chr"),
                                          evaluate_1L_lgl = FALSE)
  }
  if(what_1L_chr == "grouping"){ # get_group_by_var
    object_xx <- ifelse(group_at_geom_unit_1L_lgl,
                        ready4::get_from_lup_obj(data_lookup_tb = x %>% dplyr::filter(spatial_unit_chr == geometry_rsl_1L_chr) %>%
                                                         dplyr::filter(as.numeric(year_chr)==as.numeric(area_bndy_yr_chr)),
                                                 match_var_nm_1L_chr = "spatial_unit_chr",
                                                 match_value_xx = geometry_rsl_1L_chr,
                                                 target_var_nm_1L_chr = "var_name_chr",
                                                 evaluate_1L_lgl = FALSE),
                        ready4::get_from_lup_obj(data_lookup_tb = x,
                                                 match_var_nm_1L_chr = "spatial_unit_chr",
                                                 match_value_xx = data_rsl_1L_chr,
                                                 target_var_nm_1L_chr = "var_name_chr",
                                                 evaluate_1L_lgl = FALSE))
  }

  return(object_xx)
}

