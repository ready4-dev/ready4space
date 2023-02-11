procure.vicinity_abbreviations <- function(x,
                                           col_nm_1L_chr = "short_name_chr",
                                           value_chr){
 object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                       match_value_xx = value_chr,
                                       match_var_nm_1L_chr = col_nm_1L_chr,
                                       target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "short_name_chr","long_name_chr","short_name_chr"),
                                       evaluate_1L_lgl = FALSE)
 return(object_xx)
}
procure.vicinity_identifiers <- function(x,
                                         col_nm_1L_chr = "spatial_unit_chr",
                                         value_chr,
                                         area_bndy_yr_chr){
  object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x %>% dplyr::filter(year_chr == area_bndy_yr_chr),
                           match_value_xx = value_chr,
                           match_var_nm_1L_chr = col_nm_1L_chr,
                           target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "spatial_unit_chr","var_name_chr","spatial_unit_chr"),
                           evaluate_1L_lgl = FALSE)
  return(object_xx)
}
