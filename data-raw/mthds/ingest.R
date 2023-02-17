ingest.vicinity_processed <- function(x,
                                         col_nm_1L_chr = "main_feature_chr",
                                         match_value_xx,
                                         processed_fls_dir_1L_chr = NA_character_){
  if(!is.na(processed_fls_dir_1L_chr)){
    x <- renew(x,#add_path_col
               processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
               what_1L_chr = "shiny")
  }
  object_xx <- readRDS(ready4::get_from_lup_obj(data_lookup_tb = x, # boundary_year
                                                match_value_xx = match_value_xx,
                                                match_var_nm_1L_chr = col_nm_1L_chr,
                                                target_var_nm_1L_chr = "shiny_source_chr",
                                                evaluate_1L_lgl = FALSE))
  return(object_xx)
}
ingest.vicinity_raw <- function(x,
                                imports_chr = character(0),
                                data_type_1L_chr = character(0),
                                path_1L_chr = character(0),
                                raw_fls_dir_1L_chr = character(0),
                                processed_fls_dir_1L_chr = character(0),
                                what_1L_chr = "list",
                                write_1L_lgl = T){
  if(what_1L_chr == "list"){ # import_data
    downloaded_data_tb <- x %>%
      dplyr::filter(data_type_chr == data_type_1L_chr) %>%
      dplyr::mutate(inc_file_main_chr = ifelse(is.null(x$new_nms_for_inc_fls_ls[[1]]),
                                               inc_file_main_chr,
                                               ifelse(is.na(new_nms_for_inc_fls_ls %>% unlist()),
                                                      inc_file_main_chr,
                                                      purrr::map_chr(new_nms_for_inc_fls_ls,
                                                                     ~ .x[[1]]))))
    path_vec <- purrr::map_chr(imports_chr,
                               ~ manufacture.vicinity_raw(downloaded_data_tb,
                                                       match_value_xx = .x,
                                                       raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                                       what_1L_chr = "path"))
    r_import_path_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                                               name_chr = x$name,
                                               data_type_chr = data_type_1L_chr)
    if(data_type_1L_chr=="Geometry"){
      ingest_ls <- purrr::map(path_vec,
                              ~ {
                                if(!write_1L_lgl & file.exists(r_import_path_chr)){
                                  "SKIP_IMPORT"
                                }else{
                                  sf::st_read(dsn=.x,
                                              layer = get_name_from_path_chr(.x,
                                                                             with_ext_1L_lgl = FALSE))
                                }
                              }
      ) %>%
        stats::setNames(imports_chr)
    }else{
      ingest_ls <- purrr::map(path_vec,
                              ~ {
                                if(!write_1L_lgl & file.exists(r_import_path_chr)){
                                  "SKIP_IMPORT"
                                }else{
                                  ingest.vicinity_raw(x = downloaded_data_tb,
                                                      path_1L_chr = .x,#get_non_shape_items_for_imp
                                                      what_1L_chr = "non-shape")
                                }
                              }
      ) %>%
        stats::setNames(imports_chr)
    }
    ingest_xx <- ingest_ls
  }
  if(what_1L_chr == "non-shape"){ #get_non_shape_items_for_imp
    file_name <-  get_name_from_path_chr(path_1L_chr)
    file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, "\\.")[,2] %>%
                                                 as.vector())
    data_type_chr <- ready4::get_from_lup_obj(data_lookup_tb = x,
                                              match_value_xx = file_name,
                                              match_var_nm_1L_chr = "inc_file_main_chr",
                                              target_var_nm_1L_chr = "data_type_chr",
                                              evaluate_1L_lgl = FALSE)
    var_name_vec <- c("area_type_chr",
                      # #"area_bndy_yr_chr", ????
                      "main_feature_chr",
                      "year_chr",
                      "region")
    var_val_chr <- purrr::map_chr(var_name_vec,
                                  ~ ready4::get_from_lup_obj(data_lookup_tb = procure.vicinity_raw(x,
                                                                                                   match_value_xx = data_type_chr,
                                                                                                   what_1L_chr=="match"),
                                                             match_value_xx = file_name,
                                                             match_var_nm_1L_chr = "inc_file_main_chr",
                                                             target_var_nm_1L_chr = .x,
                                                             evaluate_1L_lgl = FALSE))
    ingest_xx <- manufacture.vicinity_raw(x,#make_import_object # could pass custom fn to this method
                                         var_val_chr = var_val_chr,
                                         path_1L_chr = path_1L_chr)
  }

  return(ingest_xx)
}
