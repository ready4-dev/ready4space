make_data_packs.ready4_sp_import_lup <- function(x,
                                                 init_lookup_r4 = NULL,
                                                 pckg_name,
                                                 raw_data_dir,
                                                 processed_dir = "data",
                                                 lup_r4_name,
                                                 crs_nbr_dbl = NA_real_,
                                                 overwrite_lgl = F){
  if(is.null(init_lookup_r4))
    init_lookup_r4 <- ready4_lookup()
  x <- x %>% add_names() %>%
    order_tb()
  lookup_tbs_r4 <- purrr::reduce(1:nrow(x),
                                 .init = init_lookup_r4,
                                 ~ ready4fun::add_all_tbs_in_r4(r4_1 = .x,
                                                                  r4_2 = x %>% dplyr::slice(.y) %>%
                                                                    write_fls_and_mk_sngl_row_data_lup(merge_with = get_merge_sf_str(lookup_r4 = .x,
                                                                                                                      sp_import_r3_slice = x %>% dplyr::slice(.y),
                                                                                                                      processed_dir = processed_dir),#merge_with_vec[.y],
                                                                                        pckg_name = pckg_name,
                                                                                        raw_data_dir = raw_data_dir,
                                                                                        processed_dir = processed_dir,
                                                                                        crs_nbr_dbl = crs_nbr_dbl,
                                                                                        overwrite_lgl = overwrite_lgl),
                                                                  r4_name = "ready4_lookup"))
  return(lookup_tbs_r4)
}
