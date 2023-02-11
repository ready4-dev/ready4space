make_data_packs.vicinity_raw <- function(x,
                                                 init_lookup_r4 = NULL,
                                                 package_1L_chr,
                                                 raw_fls_dir_1L_chr,
                                                 processed_fls_dir_1L_chr = "data",
                                                 lup_r4_name,
                                                 crs_nbr_dbl = NA_real_,
                                                 overwrite_1L_lgl = F){
  if(is.null(init_lookup_r4))
    init_lookup_r4 <- VicinityLookup()
  x <- x %>% add_names() %>%
    order_tb()
  lookup_tbs_r4 <- purrr::reduce(1:nrow(x),
                                 .init = init_lookup_r4,
                                 ~ ready4fun::add_all_tbs_in_r4(r4_1 = .x,
                                                                  r4_2 = x %>% dplyr::slice(.y) %>%
                                                                    write_fls_and_mk_sngl_row_data_lup(merge_itms_chr = get_merge_sf_str(lookup_r4 = .x,
                                                                                                                      sp_import_r3_slice = x %>% dplyr::slice(.y),
                                                                                                                      processed_fls_dir_1L_chr = processed_fls_dir_1L_chr),#merge_itms_chr_vec[.y],
                                                                                        package_1L_chr = package_1L_chr,
                                                                                        raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                                                                        processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                                                                        crs_nbr_dbl = crs_nbr_dbl,
                                                                                        overwrite_1L_lgl = overwrite_1L_lgl),
                                                                  r4_name = "VicinityLookup"))
  return(lookup_tbs_r4)
}
