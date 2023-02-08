procure.ready4_sp_data_pack_lup <- function(x,
                                             col_chr = "main_feature",
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
procure.ready4_sp_abbreviations_lup <- function(x,
                                          col_chr = "short_name",
                                          value_chr){
  ready4fun::get_from_lup(data_lookup_tb = x,
                        lookup_reference = value_chr,
                        lookup_variable = col_chr,
                        target_variable = ifelse(col_chr == "short_name","long_name","short_name"),
                        evaluate = FALSE)
}
procure.ready4_sp_uid_lup <- function(x,
                                col_chr = "spatial_unit",
                                value_chr,
                                area_bound_yr){
  ready4fun::get_from_lup(data_lookup_tb = x %>% dplyr::filter(year == area_bound_yr),
                        lookup_reference = value_chr,
                        lookup_variable = col_chr,
                        target_variable = ifelse(col_chr == "spatial_unit","var_name","spatial_unit"),
                        evaluate = FALSE)
}
