bind_isochrone_bands <- function(isochrone_bands_ls,
                                 index_1L_int,
                                 travel_mode_1L_chr){ # CHECK
  list_of_new_sfs <- purrr::map(isochrone_bands_ls,
                                ~ .x %>% purrr::pluck(index_1L_int))
  purrr::reduce(list_of_new_sfs,
                ~ sf::st_union(.x,.y)  %>%
                  dplyr::select(id,isomin,isomax,#center_value,
                                !!rlang::sym(paste0(travel_mode_1L_chr,"_times"))#id,min,max,center,drive_times
                                ))
}

