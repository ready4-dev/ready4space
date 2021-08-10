bind_sf_rows_fn_from_web <- function(...){ #https://github.com/r-spatial/sf/issues/49
  sf_list <- rlang::dots_values(...)[[1]]
  sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% sf::st_sfc()
  df <- lapply(sf_list, function(sf) sf::st_set_geometry(sf, NULL)) %>% dplyr::bind_rows()
  sf_appended <- sf::st_sf(data.frame(df, geom=sfg_list_column))
  return(sf_appended)
}
