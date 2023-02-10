add_uid_lup <- function(x_VicinityLookup#lookup_tbs_r4
                        ){
  uid_lup_r3 <- tibble::add_row(vicinity_identifiers(),
                                spatial_unit_chr = x_VicinityLookup@vicinity_raw_r3 %>% # sp_import_lup(x_VicinityLookup) %>%
                                  dplyr::pull(area_type_chr),
                                year_chr =   x_VicinityLookup@vicinity_raw_r3 %>% # sp_import_lup(x_VicinityLookup) %>%
                                  dplyr::pull(area_bndy_yr_chr), ## "All".
                                var_name_chr =  x_VicinityLookup@vicinity_raw_r3 %>% # sp_import_lup(x_VicinityLookup) %>%
                                  dplyr::pull(uid_chr))
  x_VicinityLookup <- renewSlot(x_VicinityLookup,"vicinity_identifiers_r3", uid_lup_r3) #`sp_uid_lup<-`(x_VicinityLookup, uid_lup_r3)
  return(x_VicinityLookup)
}
