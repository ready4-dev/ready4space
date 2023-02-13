ingest.vicinity_processed <- function(x,
                                         col_nm_1L_chr = "main_feature_chr",
                                         match_value_xx,
                                         processed_fls_dir_1L_chr = NA_character_){
  if(!is.na(processed_fls_dir_1L_chr)){
    x <- add_path_col(x,
                      processed_fls_dir_1L_chr = processed_fls_dir_1L_chr)
  }
  object_xx <- readRDS(ready4::get_from_lup_obj(data_lookup_tb = x, # boundary_year
                                                match_value_xx = match_value_xx,
                                                match_var_nm_1L_chr = col_nm_1L_chr,
                                                target_var_nm_1L_chr = "shiny_source_chr",
                                                evaluate_1L_lgl = FALSE))
  return(object_xx)
}
ingest.vicinity_raw <- function(x, # import_data
                                imports_chr,
                                data_type_1L_chr,
                                raw_fls_dir_1L_chr,
                                processed_fls_dir_1L_chr,
                                write_1L_lgl = T){
  downloaded_data_tb <- x %>%
    dplyr::filter(data_type_chr == data_type_1L_chr) %>%
    dplyr::mutate(inc_file_main_chr = ifelse(is.null(x$new_nms_for_inc_fls_ls[[1]]),
                                             inc_file_main_chr,
                                             ifelse(is.na(new_nms_for_inc_fls_ls %>% unlist()),
                                                    inc_file_main_chr,
                                                    purrr::map_chr(new_nms_for_inc_fls_ls,
                                                                   ~ .x[[1]]))))
  path_vec <- purrr::map_chr(imports_chr,
                             ~ get_sngl_path_for_imp(downloaded_data_tb = downloaded_data_tb %>%
                                                       dplyr::select(c(name_chr, country_chr, area_type_chr, region_chr,
                                                                       #data_type_chr,
                                                                       main_feature_chr, year_chr, inc_file_main_chr)),
                                                     match_value_xx = .x,
                                                     raw_fls_dir_1L_chr = raw_fls_dir_1L_chr))
  r_import_path_chr <- get_r_import_path_chr(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                             name_chr = x$name,
                                             data_type_chr = data_type_1L_chr)
  if(data_type_1L_chr=="Geometry"){
    ingest_ls <- purrr::map(path_vec,
                            ~ {
                              if(!write_1L_lgl & file.exists(r_import_path_chr)){
                                "SKIP_IMPORT"
                              }else{
                                sf::st_read(dsn=.x,
                                            layer = get_name_from_path_chr(.x,
                                                                           with_ext_1L_lgl = FALSE))
                              }
                            }
    ) %>%
      stats::setNames(imports_chr)
  }else{
    ingest_ls <- purrr::map(path_vec,
                            ~ {
                              if(!write_1L_lgl & file.exists(r_import_path_chr)){
                                "SKIP_IMPORT"
                              }else{
                                get_non_shape_items_for_imp(.x,
                                                            x = downloaded_data_tb)
                              }
                            }
    ) %>%
      stats::setNames(imports_chr)
  }
  return(ingest_ls)
}
