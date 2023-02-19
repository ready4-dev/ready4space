union_one_travel_time_band_across_sites <- function(time_band_ref,
                                                    one_cluster_time_bands_ls){
  list_of_new_sfs <- purrr::map(one_cluster_time_bands_ls,
                                ~ .x %>% purrr::pluck(time_band_ref))
  purrr::reduce(list_of_new_sfs,
                ~ sf::st_union(.x,.y)  %>%
                  dplyr::select(id,min,max,center,drive_times))
}

