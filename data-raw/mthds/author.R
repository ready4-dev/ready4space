author.vicinity_processed  <- function(x,#x_VicinityLookup, # simplify_geoms_in_lup
                                       path_1L_chr,
                                       crs_dbl){
  x %>% # x_VicinityLookup@vicinity_processed_r3
    dplyr::filter(main_feature_chr =="Boundary") %>%
    dplyr::pull(source_reference_chr) %>%
    purrr::walk(~ readRDS(paste0(path_1L_chr,"/",.x,".RDS")) %>%
                  transform_to_simpler_sf(crs = crs_dbl[1]) %>%
                  saveRDS(paste0(path_1L_chr,"/",.x,".RDS")))
}
author.vicinity_raw <- function(x,
                                crs_dbl = numeric(0),
                                #dir_1L_chr = character(0), ### make path ??
                                imports_ls = NULL,
                                match_vals_xx = NULL,
                                merge_itms_chr = character(0),
                                overwrite_1L_lgl = F,
                                path_1L_chr = character(0),
                                what_1L_chr = "raw"){
  files_written_lgl <- NULL
  if(what_1L_chr %in% c("processed","raw")){
    ready4use::assert_single_row_tb(x)
  }
  if(what_1L_chr == "geometry"){
    # write_procsd_geom_imp <- function(x,
    #                                   imports_ls,
    #                                   path_to_seed_sf_1L_chr,
    #                                   merge_itms_chr,
    #                                   crs_nbr_dbl,
    #                                   overwrite_1L_lgl){
      ready4use::assert_single_row_tb(x)
      if(overwrite_1L_lgl | !file.exists(path_1L_chr)){
        if(is.na(merge_itms_chr) %>% all()){
          starter_sf <- imports_ls[[1]]
        }else{
          starter_sf <- purrr::reduce(merge_itms_chr,
                                      .init = imports_ls[[1]],
                                      ~ make_intersecting_geometries(.x,
                                                                     eval(parse(text=.y)),
                                                                     crs_nbr_dbl = crs_dbl,
                                                                     validate_1L_lgl = T
                                      ))
          if((sf::st_geometry_type(starter_sf) %>% as.character()!="POINT") %>% any()){
            starter_sf <- starter_sf %>%
              dplyr::mutate(area = sf::st_area(.)) %>%
              dplyr::filter(area > units::set_units(0,m^2)) ## Note: Will discard points
          }
        }
        if(x %>% dplyr::pull(main_feature_chr) == "Boundary")
          starter_sf <- starter_sf %>%
            transform_to_simpler_sf(crs_dbl = crs_dbl[1])
        saveRDS(starter_sf, file = path_1L_chr)
      }
    # }
  }
  if(what_1L_chr == "processed"){ # write_procsd_imp_xx
    #                                 path_to_seed_sf_1L_chr, path_1L_chr
    #                                 raw_fls_dir_1L_chr, # path_1L_chr
    #                                 processed_fls_dir_1L_chr,
    #                                 crs_nbr_dbl = NA_real_,
    #                                 overwrite_1L_lgl = F){
      if(x %>% dplyr::pull(data_type_chr) == "Geometry"){
        author.vicinity_raw(x,#write_procsd_geom_imp
                              imports_ls = imports_ls,
                              path_1L_chr = path_1L_chr,
                              #processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                              merge_itms_chr = merge_itms_chr,
                              crs_dbl = crs_dbl,
                              overwrite_1L_lgl = overwrite_1L_lgl)
      }
      if(x %>% dplyr::pull(data_type_chr) == "Attribute"){
        purrr::walk2(imports_ls,
                     names(imports_ls),
                     ~ write_att_tb(att_tb = .x,
                                     object_nm_1L_chr = .y,
                                     processed_fls_dir_1L_chr = path_1L_chr,#dir_1L_chr,
                                     overwrite_1L_lgl = overwrite_1L_lgl))

      }
    # }
  }
  if(what_1L_chr == "raw"){
    files_written_lgl <- purrr::map_lgl(match_vals_xx,
                                        ~ authorData.vicinity_raw(x,
                                                                  path_1L_chr = path_1L_chr,
                                                                  data_match_value_xx = .x,
                                                                  overwrite_1L_lgl = overwrite_1L_lgl))
  }

  return(files_written_lgl)
}
