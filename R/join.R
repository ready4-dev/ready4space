join_lon_lat_sfs <- function(polys_sf,
                             points_sf,
                             crs_nbr_vec,
                             validate_lgl = T){
  sf_3 <- sf::st_join(polys_sf %>% sf::st_transform(crs_nbr_vec[2]),
                      points_sf  %>% sf::st_transform(crs_nbr_vec[2])) %>%
    sf::st_transform(crs_nbr_vec[1])
  if(validate_lgl)
    sf_3 %>% make_valid_new_sf()
  else
    sf_3
}
