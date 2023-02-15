renew_VicinityArguments <- function(x, # "makeProcessed_r4" #write_fls_from_local_imp
                                     raw_fls_dir_1L_chr,
                                     write_1L_lgl){
  x <- x %>%
    renewSlot("write_1L_lgl",
              write_1L_lgl) %>%
    renewSlot("raw_fls_dir_1L_chr",
              raw_fls_dir_1L_chr)
  return(x)
}
renew_VicinityLookup <- function(x,
                                 package_1L_chr = character(0),
                                 tbl_data_type_1L_chr = "Geometry",
                                 template_ls = NULL,
                                 what_1L_chr = "processed"){
  if(what_1L_chr = "processed"){ # add_data_pack_lup
    data_pk_lup_arguments_ls <- purrr::map2(template_ls, # remove (carefully)
                                            names(template_ls),
                                            ~ list(.x, # remove (carefully)
                                                   .y,
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "area_type_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE),
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "area_bndy_yr_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE),
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "region_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE),
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "year_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE),
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "year_start_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE),
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "year_end_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE),
                                                   ready4::get_from_lup_obj(data_lookup_tb = x@vicinity_raw_r3,
                                                                            target_var_nm_1L_chr = "main_feature_chr",
                                                                            match_var_nm_1L_chr = "name_chr",
                                                                            match_value_xx = .y,
                                                                            evaluate_1L_lgl = FALSE))) # Set names here to allow names based referencing in destination function.
    y_vicinity_processed <- purrr::reduce(data_pk_lup_arguments_ls,
                                      .init = x@vicinity_processed_r3,
                                      ~ add_attr_tb_to_processed_lup(.x,.y)) %>%
      dplyr::mutate(data_type_chr = tbl_data_type_1L_chr)
    package_1L_chr <- ifelse(package_1L_chr ==""|is.na(package_1L_chr),"", paste0(package_1L_chr,"::"))
    y_vicinity_processed <- y_vicinity_processed %>%
      dplyr::mutate(source_reference_chr = paste0(package_1L_chr,source_reference_chr))  %>%
      dplyr::mutate(source_reference_chr = purrr::map2_chr(main_feature_chr,
                                                           source_reference_chr,
                                                           ~ ifelse(.x == "Boundary",
                                                                    paste0(.y,
                                                                           "_sf"),
                                                                    .y)))
    x <- renewSlot(x,
                   "vicinity_processed_r3",#"vicinity_raw_r3",# Correct?
                   y_vicinity_processed)
  }
  return(x)
}

