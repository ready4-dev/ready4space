ingest.vicinity_processed_r3 <- function(x,
                                         col_nm_1L_chr = "main_feature_chr",
                                         value_chr,
                                         r_data_dir_chr = NA_character_){
  if(!is.na(r_data_dir_chr)){
    x <- add_path_col(x,
                      r_data_dir_chr = r_data_dir_chr)
  }
  object_xx <- readRDS(ready4::get_from_lup_obj(data_lookup_tb = x, # boundary_year
                                                match_value_xx = value_chr,
                                                match_var_nm_1L_chr = col_nm_1L_chr,
                                                target_var_nm_1L_chr = "shiny_source_chr",
                                                evaluate_1L_lgl = FALSE))
  return(object_xx)
}
