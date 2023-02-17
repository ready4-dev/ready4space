transform_sf_ls <- function(sf_ls){
  common_vars_chr <- make_common_sf_vars_ls(sf_ls)
  transformed_sf_ls <-purrr::map(sf_ls, ~ .x %>% dplyr::select(common_vars_chr))
  return(transformed_sf_ls)
}
transform_multiple_vals <- function(distribution_chr,
                                    dstr_param_1_dbl,
                                    dstr_param_2_dbl,
                                    dstr_param_3_dbl,
                                    transformation_chr){
  tfd_vas_num <- purrr::map_dbl(1:length(distribution_chr),
                                ~ transform_value(distribution_chr[.x],
                                                  dstr_param_1_dbl[.x],
                                                  dstr_param_2_dbl[.x],
                                                  dstr_param_3_dbl[.x],
                                                  transformation_chr[.x]))
  return(tfd_vas_num)
}
transform_value <- function(distribution_1L_chr, # calculate_val_from_dstr_sngl
                            dstr_param_1_1L_dbl,
                            dstr_param_2_1L_dbl,
                            dstr_param_3_1L_dbl,
                            transformation_1L_chr){
  if(distribution_chr == "none")
    x <- dstr_param_1_1L_dbl
  if(!is.na(transformation_1L_chr))
    x <- eval(parse(text=transformation_1L_chr))
  tfd_val_1L_num <- x
  return(tfd_val_1L_num)
}

## Staged

transform_circles_to_bands <- function(geom_distance_circle_sfs_list){
  purrr::map(1:(length(geom_distance_circle_sfs_list)-1),
             ~ sf::st_difference(geom_distance_circle_sfs_list %>%
                                   purrr::pluck(.x+1),
                                 geom_distance_circle_sfs_list %>%
                                   purrr::pluck(.x)) %>%
               dplyr::select(distance_in_km_dbl)) %>%
    stats::setNames(paste0("dist_"
                           ,2:(geom_distance_circle_sfs_list  %>%
                                 length())))  %>%
    purrr::prepend(list(dist_1 = geom_distance_circle_sfs_list %>%
                          purrr::pluck(1)))

}



transform_sfx_to_pfx <- function(data_tb,
                             suffix){ ##### MOVE THIS TO READY4FUN (AND UPDATE ALL REFERENCES TO THIS FUNCTION)
  data_tb %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix)),
                     dplyr::funs(paste0(suffix,
                                        "_",
                                        gsub(paste0("_",
                                                    suffix),"",.))))
}

transform_tt_polygon_to_sf <- function(tt_polyline,
                                       mode_of_transport,
                                       travel_time_hours,
                                       crs){
  test_g_polyline <- tt_polyline %>% as.character()
  googlePolylines::decode(test_g_polyline) %>%
    purrr::pluck(1) %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    data.frame(mode_of_transport = mode_of_transport,
               travel_time_hours = travel_time_hours,
               .) %>%
    sf::st_sf(crs = crs)
}
