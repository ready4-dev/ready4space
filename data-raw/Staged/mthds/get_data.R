procure.ready4_sp_data_pack_lup <- function(x,
                                             col_chr = "main_feature_chr",
                                             value_chr,
                                             r_data_dir_chr = NA_character_){
  if(!is.na(r_data_dir_chr)){
    x <- add_path_col(x,
                      r_data_dir_chr = r_data_dir_chr)
  }
  readRDS(ready4fun::get_from_lup(data_lookup_tb = x, # boundary_year
                                lookup_reference = value_chr,
                                lookup_variable = col_chr,
                                target_variable = "shiny_source",
                                evaluate = FALSE))
}
procure.vicinity_abbreviations <- function(x,
                                          col_chr = "short_name_chr",
                                          value_chr){
  ready4fun::get_from_lup(data_lookup_tb = x,
                        lookup_reference = value_chr,
                        lookup_variable = col_chr,
                        target_variable = ifelse(col_chr == "short_name_chr","long_name_chr","short_name_chr"),
                        evaluate = FALSE)
}
procure.vicinity_identifiers <- function(x,
                                col_chr = "spatial_unit_chr",
                                value_chr,
                                area_bndy_yr_chr){
  ready4fun::get_from_lup(data_lookup_tb = x %>% dplyr::filter(year_chr == area_bndy_yr_chr),
                        lookup_reference = value_chr,
                        lookup_variable = col_chr,
                        target_variable = ifelse(col_chr == "spatial_unit_chr","var_name_chr","spatial_unit_chr"),
                        evaluate = FALSE)
}
