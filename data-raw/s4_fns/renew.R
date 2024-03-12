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
                                 path_1L_chr = character(0),
                                 tbl_data_type_1L_chr = "Geometry",
                                 template_ls = NULL,
                                 what_1L_chr = "processed"){
  if(what_1L_chr == "processed"){ # add_data_pack_lup
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
                                      ~ renew.vicinity_processed(.x,.y)) %>% #add_att_tb_to_processed_lup
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
  if(what_1L_chr == "resolutions"){ # add_resolutions_lup
    dr_dp_tb <- x@vicinity_processed_r3 %>%
      dplyr::filter(main_feature_chr == "Boundary") %>%
      dplyr::select(area_type_chr,country_chr,region_chr,source_reference_chr,year_chr) %>%
      dplyr::mutate(source_reference_chr = paste0(path_1L_chr,
                                                  "/",
                                                  source_reference_chr,
                                                  ".RDS"))
    dr_dp_vec <- dr_dp_tb  %>%
      dplyr::pull(source_reference_chr)
    dr_nt_vec <- dr_dp_tb  %>%
      dplyr::pull(region_chr)
    if(any(dr_nt_vec=="National")){
      nat_sf <- readRDS(dr_dp_vec[stringr::str_which(dr_nt_vec,"National") %>% min()])
      nat_area_1L_dbl <- nat_sf %>% make_km_sqd_dbl()
    }else{
      nat_area_1L_dbl <- NA_real_
    }
    resolution_lup_r3 <- purrr::pmap_dfr(dr_dp_tb,
                                         ~ tibble::tibble(parent_area_chr= ..2,
                                                          boundary_year_dbl = as.numeric(..5),
                                                          area_type_chr = ..1,
                                                          area_count_dbl = nrow(readRDS(..4)) %>% as.double(),
                                                          complete_lgl = T,
                                                          summed_area_dbl = ifelse(..3=="National",
                                                                                   nat_area_1L_dbl,
                                                                                   readRDS(..4) %>% make_km_sqd_dbl()),
                                                          mean_size_dbl =  summed_area_dbl / area_count_dbl))
    resolution_lup_r3 <- resolution_lup_r3 %>%
      vicinity_resolutions() %>%
      dplyr::arrange(mean_size_dbl)
    x <- renewSlot(x,
                   "vicinity_resolutions_r3",
                   resolution_lup_r3)

  }
  if(what_1L_chr == "templates"){ #add_templates
    starter_sf_nm_1L_chr <- get_name_from_path_chr(path_1L_chr, with_ext_1L_lgl = F)
    starter_sf_lup_r3 <- tibble::add_row(x@vicinity_templates_r3 ,
                                         country_chr = x@vicinity_raw_r3 %>% dplyr::pull(country_chr),
                                         area_type_chr = x@vicinity_raw_r3 %>% dplyr::pull(area_type_chr),
                                         area_bndy_yr_chr = x@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr),
                                         starter_sf_nm_chr = starter_sf_nm_1L_chr,
                                         subdivision_chr = x@vicinity_raw_r3 %>% dplyr::pull(uid_chr)) ## Assumes length one list
    x <- renewSlot(x,
                   "vicinity_templates_r3",
                   starter_sf_lup_r3)


  }
  if(what_1L_chr == "identifiers"){ # add_uid_lup
    uid_lup_r3 <- tibble::add_row(vicinity_identifiers(),
                                  spatial_unit_chr = x@vicinity_raw_r3 %>% # sp_import_lup(x) %>%
                                    dplyr::pull(area_type_chr),
                                  year_chr =   x@vicinity_raw_r3 %>% # sp_import_lup(x) %>%
                                    dplyr::pull(area_bndy_yr_chr), ## "All".
                                  var_name_chr =  x@vicinity_raw_r3 %>% # sp_import_lup(x) %>%
                                    dplyr::pull(uid_chr))
    x <- renewSlot(x,"vicinity_identifiers_r3", uid_lup_r3) #`sp_uid_lup<-`(x, uid_lup_r3)
  }
  return(x)
}

