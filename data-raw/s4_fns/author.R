author_VicinityLocalRaw <- function(x, #write_fls_from_imp_and_upd_r4
                                    processed_fls_dir_1L_chr_chr,
                                    crs_nbr_dbl){
  y_VicinityLookup <- authorData(x,##author
             return_r4_1L_lgl = T) %>%
    renewSlot("processed_fls_dir_1L_chr",
              processed_fls_dir_1L_chr_chr) %>%
    author(crs_nbr_dbl = crs_nbr_dbl) %>% #import_data
    metamorphose() #update_this
    return(y_VicinityLookup)
}
author_VicinityLocalProcessed <- function(x,#import_data #write_fls_from_sp_imp_and_upd_imp_ls
                                          crs_nbr_dbl,
                                          return_r4_1L_lgl = T) {
  vicinity_raw_r3 <- x@a_VicinityLookup@vicinity_raw_r3
  ready4use::assert_single_row_tb(vicinity_raw_r3)
  imports_ls <- ingest(x = vicinity_raw_r3,#import_data
                            imports_chr = x@imports_chr,
                            data_type_1L_chr = vicinity_raw_r3$data_type,
                            raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr,
                            processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,
                            write_1L_lgl = x@write_1L_lgl) %>%
    stats::setNames(x@imports_chr)
  if(vicinity_raw_r3$data_type == "Geometry"){
    path_to_seed_sf_1L_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,
                                                    name_chr = names(imports_ls)[1],
                                                    data_type_chr = "Geometry")
  }else{
    path_to_seed_sf_1L_chr <- NA_character_
  }
  write_procsd_imp_xx(x = vicinity_raw_r3,
                      imports_ls = imports_ls,
                      path_to_seed_sf_1L_chr = path_to_seed_sf_1L_chr,
                      merge_itms_chr = x@merge_itms_chr,
                      processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr,
                      crs_nbr_dbl = crs_nbr_dbl,
                      overwrite_1L_lgl = x@overwrite_1L_lgl)
  if(return_r4_1L_lgl)
    return_xx <- renewSlot(x,
                           "path_to_seed_sf_1L_chr",
                           path_to_seed_sf_1L_chr) %>%
    renewSlot("imports_ls",
              imports_ls)
  return(return_xx)
}
