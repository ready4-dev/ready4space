transform_sf_ls <- function(sf_ls){
  common_vars_chr <- get_common_vars_sf_ls(sf_ls)
  transformed_sf_ls <-purrr::map(sf_ls, ~ .x %>% dplyr::select(common_vars_chr))
  return(transformed_sf_ls)
}
## Staged

transform_circles_to_bands <- function(geom_distance_circle_sfs_list){
  purrr::map(1:(length(geom_distance_circle_sfs_list)-1),
             ~ sf::st_difference(geom_distance_circle_sfs_list %>%
                                   purrr::pluck(.x+1),
                                 geom_distance_circle_sfs_list %>%
                                   purrr::pluck(.x)) %>%
               dplyr::select(distance_km)) %>%
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
transform_sp_local_r4_toProcessed_r4 <- function(x,
                                                   imports_chr,
                                                   raw_fls_dir_1L_chr,
                                                   write_1L_lgl){
  VicinityLocalProcessed(lup_tbs_r4 = x@lup_tbs_r4,
                       imports_chr = imports_chr,
                       merge_itms_chr = x@merge_itms_chr,
                       overwrite_1L_lgl = x@overwrite_1L_lgl,
                       raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                       write_1L_lgl = write_1L_lgl)
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
