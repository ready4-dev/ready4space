metamorphose_VicinityLocal <- function(x, #"makeProcessed_r4" #transform_sp_local_r4_toProcessed_r4
                                       imports_chr,
                                       raw_fls_dir_1L_chr,
                                       write_1L_lgl){
  x_VicinityLocalProcessed <- VicinityLocalProcessed(lup_tbs_r4 = x@lup_tbs_r4,
                                                     imports_chr = imports_chr,
                                                     merge_itms_chr = x@merge_itms_chr,
                                                     overwrite_1L_lgl = x@overwrite_1L_lgl,
                                                     raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                                     write_1L_lgl = write_1L_lgl)
  return(x_VicinityLocalProcessed)
}
metamorphose_VicinityLocalProcessed <- function(x) { #update_this #update_spProcessed_r4
  y_VicinityLookup <- x@a_VicinityLookup
  vicinity_raw_r3 <- y_VicinityLookup@vicinity_raw_r3
  ready4use::assert_single_row_tb(vicinity_raw_r3)
  if(vicinity_raw_r3$data_type_chr == "Geometry"){
    y_VicinityLookup <- renew(y_VicinityLookup,
                                      path_1L_chr = x@path_to_seed_sf_1L_chr,
                              what_1L_chr = "templates") %>%
      renew(what_1L_chr = "identifiers")#add_uid_lup()
  }
  #}
  y_VicinityLookup <- y_VicinityLookup %>%
    renew(template_ls = x@imports_ls,#add_data_pack_lup
          tbl_data_type_1L_chr = vicinity_raw_r3$data_type_chr,
          package_1L_chr = x@pkg_1L_chr,
          what_1L_chr = "processed")
  return(y_VicinityLookup)

}

