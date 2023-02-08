add_uid_lup <- function(lookup_tbs_r4){
  uid_lup_r3 <- tibble::add_row(ready4_sp_uid_lup(),
                                spatial_unit = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type),
                                year =  sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_bound_yr), ## "All".
                                var_name = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid))
  `sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
}
