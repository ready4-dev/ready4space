simplify_geoms_in_lup <- function(lup_r4,
                                  r_data_dir,
                                  crs_nbr){
  lup_r4 %>%
    sp_data_pack_lup() %>%
    dplyr::filter(main_feature =="Boundary") %>%
    dplyr::pull(source_reference) %>%
    purrr::walk(~ readRDS(paste0(r_data_dir,"/",.x,".RDS")) %>%
                  simplify_sf(crs = crs_nbr[1]) %>%
                  saveRDS(paste0(r_data_dir,"/",.x,".RDS")))
}
simplify_sf <- function(sf,
                        crs = NULL){ ## NOTE: CURRENTLY CREATES POLYGON WITH NA VALUE FEATURES TO FILL GAPS LEFT BY REMOVED LINESTRINGS
  if(is.null(crs))
    crs <- sf::st_crs(sf)[[1]]
  sf_poly <- sf %>% dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  sf_other <- sf %>%
    dplyr::filter(!sf::st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  if(nrow(sf_other)!=0){
    sf_other <- sf_other %>%
      sf::st_collection_extract()
    sf_poly <- rbind(sf_poly,sf_other)
  }
  sf_poly_json <- geojsonio::geojson_json(sf_poly, geometry = "polygon", type = "auto")
  simple_poly_json <- rmapshaper::ms_simplify(sf_poly_json)
  sf_poly <- geojsonio::geojson_sf(simple_poly_json) %>%
    dplyr::select(-rmapshaperid) %>%
    sf::st_transform(crs = crs)
}
