metamorphose.vicinity_raw <- function(x,#make_data_packs
                                      y_VicinityLookup = NULL,
                                      package_1L_chr,
                                      raw_fls_dir_1L_chr,
                                      processed_fls_dir_1L_chr = "data",
                                      lup_r4_name,
                                      crs_nbr_dbl = NA_real_,
                                      overwrite_1L_lgl = F){
  if(is.null(y_VicinityLookup))
    y_VicinityLookup <- VicinityLookup()
  x <- x %>% add_names() %>%
    order_tb()
  z_VicinityLookup <- purrr::reduce(1:nrow(x),
                                 .init = y_VicinityLookup,
                                 ~ ready4::rowbind_all_tbs_in_r4_obj(tbs_r4 = .x, #ready4fun::add_all_tbs_in_r4
                                                                     second_tbs_r4 = x %>% dplyr::slice(.y) %>%
                                                                    manufacture(merge_itms_chr = make_merge_sf_chr(.x, #write_fls_and_mk_sngl_row_data_lup
                                                                                                                  y_vicinity_raw= x %>% dplyr::slice(.y),
                                                                                                                  processed_fls_dir_1L_chr = processed_fls_dir_1L_chr),#merge_itms_chr_vec[.y],
                                                                                        package_1L_chr = package_1L_chr,
                                                                                        raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                                                                        processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                                                                        crs_nbr_dbl = crs_nbr_dbl,
                                                                                        overwrite_1L_lgl = overwrite_1L_lgl),
                                                                  r4_name_1L_chr = "VicinityLookup"))
  return(z_VicinityLookup)
}
