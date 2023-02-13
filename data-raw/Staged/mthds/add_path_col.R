add_path_col.ready4_sp_data_pack_lup <- function(x,
                                                 processed_fls_dir_1L_chr){
  x %>%
    dplyr::mutate(start_from = purrr::map_dbl(source_reference_chr, ~ 2 + stringr::str_locate(.x,":") %>% purrr::pluck(1))) %>%
    dplyr::mutate(start_from = purrr::map_dbl(start_from, ~ ifelse(is.na(.x),1,.x))) %>%
    dplyr::mutate(shiny_source_chr = paste0(processed_fls_dir_1L_chr,"/",stringr::str_sub(source_reference_chr,start=start_from),".RDS"))

}
