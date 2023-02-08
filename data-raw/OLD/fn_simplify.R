#' Simplify geoms in
#' @description simplify_geoms_in_lup() is a Simplify function that simplifies an object. Specifically, this function implements an algorithm to simplify geoms in lookup table. The function is called for its side effects and does not return a value.
#' @param lup_r4 Lookup table ready4 S4 (a ready4 S4 collection of lookup tables)
#' @param r_data_dir PARAM_DESCRIPTION
#' @param crs_nbr PARAM_DESCRIPTION
#' @return NULL
#' @rdname simplify_geoms_in_lup
#' @export 
#' @importFrom dplyr filter pull
#' @importFrom purrr walk
simplify_geoms_in_lup <- function (lup_r4, r_data_dir, crs_nbr) 
{
    lup_r4 %>% sp_data_pack_lup() %>% dplyr::filter(main_feature == 
        "Boundary") %>% dplyr::pull(source_reference) %>% purrr::walk(~readRDS(paste0(r_data_dir, 
        "/", .x, ".RDS")) %>% simplify_sf(crs = crs_nbr[1]) %>% 
        saveRDS(paste0(r_data_dir, "/", .x, ".RDS")))
}
#' Simplify
#' @description simplify_sf() is a Simplify function that simplifies an object. Specifically, this function implements an algorithm to simplify simple features object. The function is called for its side effects and does not return a value.
#' @param sf Simple features object (a simple features object)
#' @param ... Additional arguments, Default: NULL
#' @return NULL
#' @rdname simplify_sf
#' @export 
#' @importFrom sf st_crs st_geometry_type st_collection_extract st_transform
#' @importFrom dplyr filter select
#' @importFrom geojsonio geojson_json geojson_sf
#' @importFrom rmapshaper ms_simplify
simplify_sf <- function (sf, crs = NULL) 
{
    if (is.null(crs)) 
        crs <- sf::st_crs(sf)[[1]]
    sf_poly <- sf %>% dplyr::filter(sf::st_geometry_type(.) %in% 
        c("POLYGON", "MULTIPOLYGON"))
    sf_other <- sf %>% dplyr::filter(!sf::st_geometry_type(.) %in% 
        c("POLYGON", "MULTIPOLYGON"))
    if (nrow(sf_other) != 0) {
        sf_other <- sf_other %>% sf::st_collection_extract()
        sf_poly <- rbind(sf_poly, sf_other)
    }
    sf_poly_json <- geojsonio::geojson_json(sf_poly, geometry = "polygon", 
        type = "auto")
    simple_poly_json <- rmapshaper::ms_simplify(sf_poly_json)
    sf_poly <- geojsonio::geojson_sf(simple_poly_json) %>% dplyr::select(-rmapshaperid) %>% 
        sf::st_transform(crs = crs)
}
