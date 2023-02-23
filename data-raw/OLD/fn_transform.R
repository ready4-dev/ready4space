#' Transform circles to bands
#' @description transform_circles_to_bands() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform circles to bands. Function argument geomc_dist_circles_ls specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param geomc_dist_circles_ls PARAM_DESCRIPTION
#' @return NULL
#' @rdname transform_circles_to_bands
#' @export 
#' @importFrom purrr map pluck prepend
#' @importFrom sf st_difference
#' @importFrom dplyr select
#' @importFrom stats setNames
transform_circles_to_bands <- function (geomc_dist_circles_ls) 
{
    purrr::map(1:(length(geomc_dist_circles_ls) - 1), 
        ~sf::st_difference(geomc_dist_circles_ls %>% 
            purrr::pluck(.x + 1), geomc_dist_circles_ls %>% 
            purrr::pluck(.x)) %>% dplyr::select(distance_km)) %>% 
        stats::setNames(paste0("dist_", 2:(geomc_dist_circles_ls %>% 
            length()))) %>% purrr::prepend(list(dist_1 = geomc_dist_circles_ls %>% 
        purrr::pluck(1)))
}
#' Transform suffix to prefix
#' @description transform_sfx_to_pfx() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform suffix to prefix. Function argument data_tb specifies the object to be updated. Argument suffix provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param data_tb Data (a tibble)
#' @param suffix PARAM_DESCRIPTION
#' @return NULL
#' @rdname transform_sfx_to_pfx
#' @export 
#' @importFrom dplyr rename_at vars ends_with funs
transform_sfx_to_pfx <- function (data_tb, suffix) 
{
    data_tb %>% dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix)), 
        dplyr::funs(paste0(suffix, "_", gsub(paste0("_", suffix), 
            "", .))))
}
#' Transform sp local ready4 S4 to local process
#' @description transform_sp_local_r4_toProcessed_r4() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform sp local ready4 s4 to local process ready4 s4. Function argument x specifies the object to be updated. Argument imports_chr provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param imports_chr PARAM_DESCRIPTION
#' @param raw_fls_dir_1L_chr Raw data directory (a character vector)
#' @param write_1L_lgl Save (a logical vector)
#' @return NULL
#' @rdname transform_sp_local_r4_toProcessed_r4
#' @export 

transform_sp_local_r4_toProcessed_r4 <- function (x, imports_chr, raw_fls_dir_1L_chr, write_1L_lgl) 
{
    VicinityLocalProcessed(lup_tbs_r4 = x@lup_tbs_r4, imports_chr = imports_chr, 
        merge_itms_chr = x@merge_itms_chr, overwrite_1L_lgl = x@overwrite_1L_lgl, 
        raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, write_1L_lgl = write_1L_lgl)
}
#' Transform tt polygon to
#' @description transform_polyline_to_sf() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform tt polygon to simple features object. Function argument polyline_xx specifies the object to be updated. Argument mode_of_transport_1L_chr provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param polyline_xx PARAM_DESCRIPTION
#' @param mode_of_transport_1L_chr PARAM_DESCRIPTION
#' @param travel_time_hours PARAM_DESCRIPTION
#' @param ... Additional arguments
#' @return NULL
#' @rdname transform_polyline_to_sf
#' @export 
#' @importFrom googlePolylines decode
#' @importFrom purrr pluck
#' @importFrom sf st_polygon st_sfc st_sf
transform_polyline_to_sf <- function (polyline_xx, mode_of_transport_1L_chr, travel_time_hours, 
    crs) 
{
    test_g_polyline <- polyline_xx %>% as.character()
    googlePolylines::decode(test_g_polyline) %>% purrr::pluck(1) %>% 
        as.matrix() %>% list() %>% sf::st_polygon() %>% sf::st_sfc() %>% 
        data.frame(mode_of_transport_1L_chr = mode_of_transport_1L_chr, travel_time_hours = travel_time_hours, 
            .) %>% sf::st_sf(crs = crs)
}
