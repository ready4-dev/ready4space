add_attr_list_to_sf <- function(x,
                                y,
                                lookup_tb_r4){
  attr_data_xx <- make_attr_data_xx(lookup_tb_r4 = lookup_tb_r4,
                                    lookup_ref = y,
                                    starter_sf = x)
  add_attr_to_sf(area_sf = x,
                 attr_data_tb = attr_data_xx,
                 attr_data_desc = ready4fun::get_from_lup(data_lookup_tb = sp_data_pack_lup(lookup_tb_r4),
                                                          lookup_reference = y,
                                                          lookup_variable = "name",
                                                          target_variable = "main_feature_chr",
                                                          evaluate = FALSE)
  )
}
add_attribute_to_data_pack_from_tb <- function(attr_tb,
                                               object_name){
  eval(parse(text = paste0(object_name,
                           "<<-attr_tb")))
  eval(parse(text = paste0("usethis::use_data(",object_name,
                           ", overwrite = TRUE)")))
}
add_attribute_to_data_pack <- function(combined_ste_ppr_ls,
                                       object_name_stub){
  add_attr_to_global(combined_ste_ppr_ls = combined_ste_ppr_ls,
                           object_name_stub = object_name_stub)
  purrr::walk(names(combined_ste_ppr_ls),
              ~ eval(parse(text = paste0("usethis::use_data(",object_name_stub,.x %>% stringr::str_sub(start = 2),
                                         ", overwrite = TRUE)"))))
}
add_attr_recrly_to_sf <- function(input_ls,
                                  sub_div_unit = NULL,
                                  area_unit,
                                  boundary_year_1L_chr,
                                  attribute_data
){
  lookup_tb_r4 <- lookup_tb(input_ls$pa_r4)
  data_lookup_tb <- sp_data_pack_lup(lookup_tb_r4)
  boundary_file <- procure(data_lookup_tb %>%
                              dplyr::filter(area_type_chr == area_unit) %>%
                              dplyr::filter(main_feature_chr == "Boundary") %>%
                              dplyr::filter(as.numeric(year_start_chr) == max(as.numeric(year_start_chr)[as.numeric(year_start_chr) <= as.numeric(boundary_year_1L_chr)])),
                            value_chr = "Boundary")
  attribute_data_list <- purrr::map(attribute_data,
                                    ~ .x) %>%
    stats::setNames(attribute_data)
  purrr::map(attribute_data_list, ~ add_attr_list_to_sf(x = boundary_file,
                                                        y = .x,
                                                        lookup_tb_r4 = lookup_tb_r4)) %>%
    subset_sf_ls_by_common_vars() %>%
    purrr::reduce(~rbind(.x,.y))
}
add_attr_tb_to_data_pack_lup_from_arg_list <- function(x,y){ ## Replace with names based referencing.
  add_attr_tb_to_data_pack_lup(data_pack_lup = x,
                               attr_tb = y[[1]], # remove (carefully)
                               object_name = y[[2]],
                               area_type_chr = y[[3]],
                               area_bndy_yr_chr = y[[4]],
                               region = y[[5]],
                               year_chr = y[[6]],
                               year_start_chr = y[[7]],
                               year_end_chr = y[[8]],
                               main_feature_chr = y[[9]])
}
add_attr_tb_to_data_pack_lup <- function(data_pack_lup,
                                         attr_tb, # remove (carefully)
                                         object_name,
                                         area_type_chr,
                                         area_bndy_yr_chr,
                                         region,
                                         year_chr,
                                         year_start_chr,
                                         year_end_chr,
                                         main_feature_chr){ # replace with names based referencing
  tibble::tibble(name = object_name,
                 country_chr = "Australia", # Pull this from context data.
                 area_type_chr = area_type_chr,
                 area_bndy_yr_chr = area_bndy_yr_chr,
                 region = region,
                 data_type_chr = "Attribute",
                 main_feature_chr = main_feature_chr,
                 year_chr = year_chr,
                 year_start_chr = year_start_chr,
                 year_end_chr = year_end_chr,
                 source_reference_chr = object_name) %>%
    dplyr::bind_rows(data_pack_lup,
                     .)
}
add_attr_to_global <- function(combined_ste_ppr_ls,
                                     object_name_stub){
  purrr::walk(names(combined_ste_ppr_ls),
              ~ eval(parse(text = paste0(object_name_stub,.x %>% stringr::str_sub(start = 2),
                                         "<<-combined_ste_ppr_ls$",.x))))
}
add_attr_to_sf <- function(#area_unit,
  area_sf,
  attr_data_tb,
  attr_data_desc#,
  #attr_data_year,
  #boundary_year,
  #sub_div_unit
){
  if(attr_data_desc == "PPR"){ # "Population projections"
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb)
  }
  if(stringr::str_detect(attr_data_desc, "ERP_TOT")){ #"ERP by age and sex"
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  if(attr_data_desc == "ERP_ASX"){ # "ERP"
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  return(merged_units)
}
add_data_pack_lup <- function(lookup_tbs_r4,
                              tb_data_type = "Geometry",
                              template_ls = NULL,
                              pckg_name){
  data_pk_lup_arguments_ls <- purrr::map2(template_ls, # remove (carefully)
                                          names(template_ls),
                                          ~ list(.x, # remove (carefully)
                                                 .y,
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "area_type_chr",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE),
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "area_bndy_yr_chr",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE),
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "region",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE),
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "year_chr",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE),
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "year_start_chr",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE),
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "year_end_chr",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE),
                                                 ready4fun::get_from_lup(data_lookup_tb = lookup_tbs_r4 %>%
                                                                           sp_import_lup(),
                                                                         target_variable = "main_feature_chr",
                                                                         lookup_variable = "name",
                                                                         lookup_reference = .y,
                                                                         evaluate = FALSE))) # Set names here to allow names based referencing in destination function.
  data_pack_lup_r3 <- purrr::reduce(data_pk_lup_arguments_ls,
                                    .init = lookup_tbs_r4 %>%
                                      sp_data_pack_lup(),
                                    ~ add_attr_tb_to_data_pack_lup_from_arg_list(.x,.y)) %>%
    dplyr::mutate(data_type_chr = tb_data_type)
  pckg_name <- ifelse(pckg_name ==""|is.na(pckg_name),"", paste0(pckg_name,"::"))
  data_pack_lup_r3 <- data_pack_lup_r3 %>%
    dplyr::mutate(source_reference_chr = paste0(pckg_name,source_reference_chr))  %>%
    dplyr::mutate(source_reference_chr = purrr::map2_chr(main_feature_chr,
                                                     source_reference_chr,
                                                     ~ ifelse(.x == "Boundary",
                                                              paste0(.y,
                                                                     "_sf"),
                                                              .y)))
  lookup_tbs_r4 <- `sp_data_pack_lup<-`(lookup_tbs_r4, data_pack_lup_r3)
  lookup_tbs_r4
}
add_dynamic_sp_vars_to_sf <- function(dynamic_sp_vars_sf,
                                      pop_attr_sf,
                                      age_sex_pop_resolution,
                                      age_sex_var_name,
                                      popl_var_prefix,
                                      data_year_chr,
                                      crs_nbr_dbl){
  profiled_sf <- intersect_sfs_keep_counts(profiled_sf = dynamic_sp_vars_sf,
                                           profiled_colref = NA,
                                           profiled_rowref = NA,
                                           attribute_sf = pop_attr_sf,
                                           attribute_unit = age_sex_pop_resolution,
                                           data_type_chr = "processed_age_sex",
                                           data_year_chr = data_year_chr,
                                           popl_var_prefix = popl_var_prefix,
                                           crs_nbr_dbl = crs_nbr_dbl) %>%
    add_kmsq_area_by_group(group_by_var = age_sex_var_name,
                           feature_nm = age_sex_pop_resolution)
  dyn_param_unit_id <- names(dynamic_sp_vars_sf)[1] # Should be read from lookup
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(age_sex_var_name) := paste0(!!rlang::sym(dyn_param_unit_id),"_",!!rlang::sym(age_sex_var_name)))
  update_pop_count_by_areas(profiled_sf = profiled_sf,
                            group_by_var = group_by_var,
                            age_sex_var_name = age_sex_var_name,
                            data_year_chr = data_year_chr,
                            age_sex_pop_resolution = age_sex_pop_resolution,
                            tot_pop_resolution = NULL,
                            popl_var_prefix = popl_var_prefix)

}
add_kmsq_area_all_features <- function(sf,
                                       feature_nm,
                                       prefix = "whl_",
                                       suffix = "_area"){
  sf %>%
    dplyr::mutate(!!rlang::sym(paste0(prefix,
                                      feature_nm,
                                      suffix)) := sf::st_area(.) %>%
                    units::set_units(km^2))
}
add_kmsq_area_by_group <- function(sf,
                                   group_by_var,
                                   feature_nm,
                                   prefix = "whl_",
                                   suffix = "_area"){
  merge(sf,
        sf %>%
          dplyr::group_by(!!rlang::sym(group_by_var)) %>%
          dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
          add_kmsq_area_all_features(feature_nm = feature_nm,
                                     prefix = prefix,
                                     suffix = suffix) %>%
          dplyr::ungroup() %>%
          sf::st_set_geometry(NULL))

}
add_names <- function(x){
  data(ISO_3166_1, package = "ISOcodes", envir = environment())
  x <- x %>%
    dplyr::mutate(name = purrr::pmap_chr(list(country_chr,
                                              area_type_chr,
                                              region,
                                              data_type_chr,
                                              main_feature_chr,
                                              year_chr),
                                         ~ paste0(ready4fun::get_from_lup(data_lookup_tb = ISO_3166_1,
                                                                          lookup_reference = ..1,
                                                                          lookup_variable = "Name",
                                                                          target_variable = "Alpha_3",
                                                                          evaluate = FALSE) %>% tolower(),
                                                  "_",
                                                  tolower(..2),
                                                  "_",
                                                  tolower(..3 %>% stringr::str_sub(end=3)),
                                                  ifelse(..4 == "Geometry",
                                                         ifelse(..5 == "Boundary","_bnd_","_crd_"),
                                                         paste0("_",tolower(..5),"_")),
                                                  ..6
                                         )))
  x %>% dplyr::mutate(name = make.unique(name)) %>% dplyr::mutate(name = map_chr(name, ~ ifelse(stringr::str_sub(.x,start = -2, end = -2) == ".",
                                                                                                paste0(stringr::str_sub(.x, end = 11),
                                                                                                       stringr::str_sub(.x,start = -1),
                                                                                                       stringr::str_sub(.x, start = 12, end = -3)),
                                                                                                .x)))
}
add_ppr_ls_to_data_pack_lup <- function(x,y){ ## update with names and check calling function(s).
  add_ppr_to_data_pack_lup(data_pack_lup = x,
                           combined_ste_ppr_ls = y[[1]],
                           object_name_stub = y[[2]],
                           area_type_chr = y[[3]],
                           area_bndy_yr_chr = y[[4]],
                           region = y[[5]])
}
add_ppr_to_data_pack_lup <- function(data_pack_lup,
                                     combined_ste_ppr_ls,
                                     object_name_stub,
                                     area_type_chr,
                                     area_bndy_yr_chr,
                                     region){
  tibble::tibble(name = paste0(object_name_stub,names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2)),
                 country_chr = "Australia", # Change to context based method (lookup table)
                 area_type_chr = area_type_chr,
                 area_bndy_yr_chr = area_bndy_yr_chr,
                 region = region,
                 data_type_chr = "Attribute",
                 main_feature_chr = "Population projections",
                 year_chr = names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2),
                 source_reference_chr = paste0(object_name_stub,names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2))) %>%
    dplyr::bind_rows(data_pack_lup,
                     .)
}
add_sp_resolution <- function(lookup_tbs_r4,
                              processed_dir){
  dr_dp_tb <- sp_data_pack_lup(lookup_tbs_r4) %>%
    dplyr::filter(main_feature_chr == "Boundary") %>%
    dplyr::select(area_type_chr,country_chr,region,source_reference_chr,year_chr) %>%
    dplyr::mutate(source_reference_chr = paste0(processed_dir,
                                            "/",
                                            source_reference_chr,
                                            ".RDS"))
  dr_dp_vec <- dr_dp_tb  %>%
    dplyr::pull(source_reference_chr)
  dr_nt_vec <- dr_dp_tb  %>%
    dplyr::pull(region)
  if(any(dr_nt_vec=="National")){
    nat_sf <- readRDS(dr_dp_vec[stringr::str_which(dr_nt_vec,"National") %>% min()])
    nat_area <- nat_sf %>% get_area_sqkm_sf()
  }else{
    nat_area <- NA_real_
  }
  resolution_lup_r3 <- purrr::pmap_dfr(dr_dp_tb,
                                       ~ tibble::tibble(parent_area_chr= ..2,
                                                        boundary_year_dbl = as.numeric(..5),
                                                        area_type_chr = ..1,
                                                        area_count_dbl = nrow(readRDS(..4)) %>% as.double(),
                                                        complete_lgl = T,
                                                        summed_area_dbl = ifelse(..3=="National",
                                                                             nat_area,
                                                                             readRDS(..4) %>% get_area_sqkm_sf()),
                                                        mean_size_dbl =  summed_area_dbl / area_count_dbl))
  resolution_lup_r3 <- resolution_lup_r3 %>%
    vicinity_resolutions() %>%
    dplyr::arrange(mean_size_dbl)
  `sp_resolution_lup<-`(lookup_tbs_r4, resolution_lup_r3)
}
add_starter_sf_to_lups <- function(lookup_tbs_r4,
                                   path_to_seed_sf_1L_chr){
  starter_sf_name <- get_name_from_path_chr(path_to_seed_sf_1L_chr, with_ext = F)
  starter_sf_lup_r3 <- tibble::add_row(lookup_tbs_r4@vicinity_templates_r3 ,
                                       country_chr = lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(country_chr),
                                       area_type_chr = lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(area_type_chr),
                                       area_bndy_yr_chr = lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr),
                                       starter_sf = starter_sf_name,
                                       subdivision_chr = lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(uid)) ## Assumes length one list
  `sp_starter_sf_lup<-`(lookup_tbs_r4, starter_sf_lup_r3)
}
add_uid_lup <- function(lookup_tbs_r4){
  uid_lup_r3 <- tibble::add_row(vicinity_identifiers(),
                                spatial_unit_chr = lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(area_type_chr),
                                year_chr =  lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(area_bndy_yr_chr), ## "All".
                                var_name_chr = lookup_tbs_r4@vicinity_raw_r3 %>% dplyr::pull(uid_chr))
  `sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
}
