renew.vicinity_processed <- function(x, ## add_attr_tb_to_processed_lup & add_attrs_to_processed_lup
                                     args_ls = NULL,
                                     area_type_chr = character(0),
                                     area_bndy_yr_chr = character(0),
                                     country_chr = character(0),
                                     data_type_chr = character(0),
                                     main_feature_chr = character(0),
                                     name_chr = character(0),
                                     region_chr = character(0),
                                     source_reference_chr = character(0),
                                     year_chr = character(0),
                                     year_end_chr = ycharacter(0),
                                     year_start_chr = character(0),
                                     what_1L_chr = "table"){
  if(what_1L_chr == "names"){
    x <- add_names(x)
  }
  if(what_1L_chr == "table"){
    if(!is.null(args_ls)){
      x <- add_attrs_to_processed_lup(data_pack_lup = x,
                                      #attr_tb = args_ls[[1]], # remove (carefully)
                                      object_name_1L_chr = args_ls[[2]],
                                      area_type_chr = args_ls[[3]],
                                      area_bndy_yr_chr = args_ls[[4]],
                                      region_chr = args_ls[[5]],
                                      year_chr = args_ls[[6]],
                                      year_start_chr = args_ls[[7]],
                                      year_end_chr = args_ls[[8]],
                                      main_feature_chr = args_ls[[9]])
    }else{
      fn_env_ls <- as.list(rlang::current_env())[-1]
      x <- ready4::update_tb_r3(x,
                                fn = renew.vicinity_processed,
                                fn_env_ls = fn_env_ls)

    }
  }
  return(x)
}
renew.vicinity_raw <- function(x, ##
                               #args_ls = NULL,
                               area_type_chr = character(0),
                               area_bndy_yr_chr = character(0),
                               country_chr = character(0),
                               data_type_chr = character(0),
                               main_feature_chr = character(0),
                               name_chr = character(0),
                               processed_fls_dir_1L_chr = character(0),
                               region_chr = character(0),
                               source_reference_chr = character(0),
                               year_chr = character(0),
                               year_end_chr = ycharacter(0),
                               year_start_chr = character(0),
                               what_1L_chr = "table"){
  if(what_1L_chr == "names"){
    x <- add_names(x)
  }
  if(what_1L_chr == "order"){ #order_tb mthd # Could make into a fn if required for vicinity_processed
    not_to_be_ordered_tb <- x %>% dplyr::filter(is.na(uid_chr))
    x <- x %>% dplyr::filter(!is.na(uid_chr))
    ordering_tb <- x %>%
      dplyr::select(name_chr,uid_chr,add_boundaries_chr) %>%
      dplyr::mutate(preceeded_by = purrr::map(add_boundaries_chr,
                                              ~ unlist(.x)[unlist(.x) %in% uid_chr])) %>%
      dplyr::mutate(sequence = purrr::map2(preceeded_by,
                                           uid_chr,
                                           ~ c(.x,.y)))
    if(nrow(x) > 0){
      ordering_chr <- purrr::reduce(ordering_tb %>%
                                      dplyr::pull(sequence),
                                    ~ append(.x,.y[!.y %in% .x]))

      x <- x[match(ordering_chr, x$uid_chr),]
    }
    dplyr::bind_rows(x,not_to_be_ordered_tb)
  }
  if(what_1L_chr == "shiny"){ # add_path_col
    x <- x %>%
      dplyr::mutate(start_from = purrr::map_dbl(source_reference_chr, ~ 2 + stringr::str_locate(.x,":") %>% purrr::pluck(1))) %>%
      dplyr::mutate(start_from = purrr::map_dbl(start_from, ~ ifelse(is.na(.x),1,.x))) %>%
      dplyr::mutate(shiny_source_chr = paste0(processed_fls_dir_1L_chr,"/",stringr::str_sub(source_reference_chr,start=start_from),".RDS"))
  }
  if(what_1L_chr == "table"){
      fn_env_ls <- as.list(rlang::current_env())[-1]
      x <- ready4::update_tb_r3(x,
                                fn = renew.vicinity_raw,
                                fn_env_ls = fn_env_ls)
  }

  return(x)
}
renew.vicinity_points <- function(x,
                                  service_type_chr = character(0),
                                  cluster_name_chr = character(0),
                                  service_name_chr = character(0),
                                  lat_dbl = numeric(0),
                                  lng_dbl = numeric(0),
                                  what_1L_chr = "table",
                                  ...){
  if(what_1L_chr == "table"){
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x,
                              fn = renew.vicinity_points,
                              fn_env_ls = fn_env_ls)
  }
  return(x)
}
