author.vicinity_processed  <- function(x,#x_VicinityLookup, # simplify_geoms_in_lup
                                       dir_1L_chr,
                                       crs_dbl){
  x %>% # x_VicinityLookup@vicinity_processed_r3
    dplyr::filter(main_feature_chr =="Boundary") %>%
    dplyr::pull(source_reference_chr) %>%
    purrr::walk(~ readRDS(paste0(dir_1L_chr,"/",.x,".RDS")) %>%
                  transform_to_simpler_sf(crs = crs_dbl[1]) %>%
                  saveRDS(paste0(dir_1L_chr,"/",.x,".RDS")))
}

author.vicinity_raw <- function(x,
                                match_vals_xx,
                                dir_1L_chr,
                                overwrite_1L_lgl = F){
  files_written_lgl <- purrr::map_lgl(match_vals_xx,
                                      ~ authorData(x = x,
                                                   dir_1L_chr = dir_1L_chr,
                                                   data_match_value_xx = .x,
                                                   overwrite_1L_lgl = overwrite_1L_lgl))
  return(files_written_lgl)
}
