transform_circles_to_bands <- function(geomc_dist_circles_ls){
  bands_ls <- purrr::map(1:(length(geomc_dist_circles_ls)-1),
                         ~ sf::st_difference(geomc_dist_circles_ls %>%
                                               purrr::pluck(.x+1),
                                             geomc_dist_circles_ls %>%
                                               purrr::pluck(.x)) %>%
                           dplyr::select(distance_in_km_dbl)) %>%
    stats::setNames(paste0("dist_"
                           ,2:(geomc_dist_circles_ls  %>%
                                 length())))  %>%
    append(list(dist_1 = geomc_dist_circles_ls %>%
                  purrr::pluck(1)),
           after = 0)
  return(bands_ls)
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
transform_shape_of_popl_predns_ds <- function(popl_predns_ds,
                                              prefix_1L_chr = "Persons."){
  reshaped_tb <- t(popl_predns_ds)
  colnames(reshaped_tb) <- paste0(prefix_1L_chr,#"Persons.",
                                  as.character(unlist(reshaped_tb[1,])))
  reshaped_tb <- reshaped_tb[-1,]
  reshaped_tb<- cbind(tibble::tibble(year_chr = row.names(reshaped_tb)),
                      reshaped_tb) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.factor,
                     .funs = as.character) %>%
    dplyr::mutate_if(is.character,
                     .funs = stringr::str_trim)
  reshaped_tb <- reshaped_tb %>%
    dplyr::mutate_at(.vars = names(reshaped_tb)[!names(reshaped_tb) %in% reshaped_tb$year_chr],
                     .funs = as.numeric)
  return(reshaped_tb)
}
transform_sf_ls <- function(sf_ls){
  common_vars_chr <- make_common_sf_vars_ls(sf_ls)
  transformed_sf_ls <-purrr::map(sf_ls, ~ .x %>% dplyr::select(common_vars_chr))
  return(transformed_sf_ls)
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
transform_sfx_to_pfx <- function(data_tb,
                                 suffix_1L_chr){ ##### MOVE THIS TO READY4FUN (AND UPDATE ALL REFERENCES TO THIS FUNCTION)
  data_tb <- data_tb %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix_1L_chr)),
                     dplyr::funs(paste0(suffix_1L_chr,
                                        "_",
                                        gsub(paste0("_",
                                                    suffix_1L_chr),"",.))))
  return(data_tb)
}
transform_to_simpler_sf <- function(sf,
                                    crs_dbl = NULL){ ## NOTE: CURRENTLY CREATES POLYGON WITH NA VALUE FEATURES TO FILL GAPS LEFT BY REMOVED LINESTRINGS
  if(is.null(crs_dbl))
    crs_dbl <- sf::st_crs(sf)[[1]]
  polygons_sf <- sf %>% dplyr::filter(sf::st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  sf_other <- sf %>%
    dplyr::filter(!sf::st_geometry_type(.) %in% c("POLYGON","MULTIPOLYGON"))
  if(nrow(sf_other)!=0){
    sf_other <- sf_other %>%
      sf::st_collection_extract()
    polygons_sf <- rbind(polygons_sf,sf_other)
  }
  polygons_sf_json <- geojsonio::geojson_json(polygons_sf, geometry = "polygon", type = "auto")
  simple_poly_json <- rmapshaper::ms_simplify(polygons_sf_json)
  polygons_sf <- geojsonio::geojson_sf(simple_poly_json) %>%
    dplyr::select(-rmapshaperid) %>%
    sf::st_transform(crs = crs_dbl)
  return(polygons_sf)
}
transform_polyline_to_sf <- function(polyline_xx,
                                     crs_1L_dbl,
                                     mode_of_transport_1L_chr,
                                     travel_time_hours_1L_dbl
                                     ){
  test_g_polyline <- polyline_xx %>% as.character()
  geometry_sf <- googlePolylines::decode(test_g_polyline) %>%
    purrr::pluck(1) %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    data.frame(mode_of_transport_chr = mode_of_transport_1L_chr,
               travel_time_hours_dbl = c,
               .) %>%
    sf::st_sf(crs = crs_1L_dbl)
  return(geometry_sf)
}
