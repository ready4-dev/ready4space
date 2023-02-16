procure.vicinity_abbreviations <- function(x,
                                           col_nm_1L_chr = "short_name_chr",
                                           match_value_xx){
 object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                       match_value_xx = match_value_xx,
                                       match_var_nm_1L_chr = col_nm_1L_chr,
                                       target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "short_name_chr","long_name_chr","short_name_chr"),
                                       evaluate_1L_lgl = FALSE)
 return(object_xx)
}
procure.vicinity_identifiers <- function(x,
                                         col_nm_1L_chr = "spatial_unit_chr",
                                         geometry_rsl_1L_chr = character(0),
                                         group_at_geom_unit_1L_lgl = TRUE,
                                         data_rsl_1L_chr = character(0),
                                         match_value_xx = NULL,
                                         area_bndy_yr_chr = character(0),
                                         what_1L_chr = "match"){
  if(what_1L_chr == "match"){
    object_xx <- ready4::get_from_lup_obj(data_lookup_tb = x %>% dplyr::filter(year_chr == area_bndy_yr_chr),
                                          match_value_xx = match_value_xx,
                                          match_var_nm_1L_chr = col_nm_1L_chr,
                                          target_var_nm_1L_chr = ifelse(col_nm_1L_chr == "spatial_unit_chr","var_name_chr","spatial_unit_chr"),
                                          evaluate_1L_lgl = FALSE)
  }
  if(what_1L_chr == "grouping"){ # get_group_by_var
    object_xx <- ifelse(group_at_geom_unit_1L_lgl,
                        ready4::get_from_lup_obj(data_lookup_tb = x %>% dplyr::filter(spatial_unit_chr == geometry_rsl_1L_chr) %>%
                                                         dplyr::filter(as.numeric(year_chr)==as.numeric(area_bndy_yr_chr)),
                                                 match_var_nm_1L_chr = "spatial_unit_chr",
                                                 match_value_xx = geometry_rsl_1L_chr,
                                                 target_var_nm_1L_chr = "var_name_chr",
                                                 evaluate_1L_lgl = FALSE),
                        ready4::get_from_lup_obj(data_lookup_tb = x,
                                                 match_var_nm_1L_chr = "spatial_unit_chr",
                                                 match_value_xx = data_rsl_1L_chr,
                                                 target_var_nm_1L_chr = "var_name_chr",
                                                 evaluate_1L_lgl = FALSE))
  }

  return(object_xx)
}
procure.vicinity_raw <- function(x, # From ready4use procure::ready4use_import
                                 inc_script_1L_lgl = T,
                                 forced_choice_chr = NA_character_,
                                 what_1L_chr = "source"){
  if(what_1L_chr == "source"){
    ready4use::assert_single_row_tb(x)
    source_ls <- list(script_chr = x$path_to_make_script_chr,
                       local_chr = x$local_file_src_chr,
                       repo_chr = x$data_repo_db_ui_chr,
                       source_url_chr = x$download_url_chr) %>% purrr::discard(is.na)
    if ("script_chr" %in% names(source_ls) & !inc_script_1L_lgl)
      source_ls$script_chr <- NULL
    if (!is.na(forced_choice_chr)) {
      if (!forced_choice_chr %in% names(source_ls))
        stop("Forced choice option is not available from input lookup table")
      source_ls <- source_ls[names(source_ls) == forced_choice_chr]
    }
    object_xx <- source_ls[1]
  }
  return(object_xx)
}
procure.vicinity_resolutions <- function(x,
                                         options_chr = character(0),
                                         what_1L_chr = "resolution",
                                         whole_area_1L_lgl = TRUE,
                                         year_1L_dbl = numeric(0)){
  if(what_1L_chr == "resolution"){
    if(!is.na(options_chr[1])){ # get_highest_res
      resolutions_chr <- procure.vicinity_resolutions(x,
                                                      year_1L_dbl = as.numeric(year_1L_dbl),
                                                      what_1L_chr = "hierarchy")#get_resolution_hierarchy
      resolution_1L_chr <- resolutions_chr[min(which(resolutions_chr %in% options_chr))]
    }else{
      resolution_1L_chr <- NA_character_
    }
    resolution_xx <- resolution_1L_chr
  }
  if(what_1L_chr == "hierarchy"){#get_resolution_hierarchy
    resolution_hierarchy <- x  %>%
      dplyr::filter(boundary_year_dbl == year_1L_dbl)
    if(whole_area_1L_lgl){
      resolution_hierarchy <- resolution_hierarchy %>%
        dplyr::filter(complete_lgl==TRUE)
    }
    resolution_xx <- resolution_hierarchy %>%
      dplyr::arrange(dplyr::desc(area_count_dbl)) %>%
      dplyr::pull(area_type_chr)

  }
  return(resolution_xx)
}
