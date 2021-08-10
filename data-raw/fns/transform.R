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
transform_sp_local_r4_to_local_proc_r4 <- function(x,
                                                   import_chr_vec,
                                                   raw_data_dir_chr,
                                                   save_lgl){
  ready4_sp_local_proc(lup_tbs_r4 = x@lup_tbs_r4,
                       import_chr_vec = import_chr_vec,
                       merge_with_chr_vec = x@merge_with_chr_vec,
                       overwrite_lgl = x@overwrite_lgl,
                       raw_data_dir_chr = raw_data_dir_chr,
                       save_lgl = save_lgl)
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
