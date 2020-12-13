#' Transform circles to bands
#' @description transform_circles_to_bands() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform circles to bands. Function argument geom_distance_circle_sfs_list specifies the object to be updated. The function is called for its side effects and does not return a value.
#' @param geom_distance_circle_sfs_list PARAM_DESCRIPTION
#' @return NULL
#' @rdname transform_circles_to_bands
#' @export 
#' @importFrom purrr map pluck prepend
#' @importFrom sf st_difference
#' @importFrom dplyr select
#' @importFrom stats setNames
#' @keywords internal
transform_circles_to_bands <- function (geom_distance_circle_sfs_list) 
{
    purrr::map(1:(length(geom_distance_circle_sfs_list) - 1), 
        ~sf::st_difference(geom_distance_circle_sfs_list %>% 
            purrr::pluck(.x + 1), geom_distance_circle_sfs_list %>% 
            purrr::pluck(.x)) %>% dplyr::select(distance_km)) %>% 
        stats::setNames(paste0("dist_", 2:(geom_distance_circle_sfs_list %>% 
            length()))) %>% purrr::prepend(list(dist_1 = geom_distance_circle_sfs_list %>% 
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
#' @keywords internal
transform_sfx_to_pfx <- function (data_tb, suffix) 
{
    data_tb %>% dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix)), 
        dplyr::funs(paste0(suffix, "_", gsub(paste0("_", suffix), 
            "", .))))
}
#' Transform sp local ready4 S4 to local process
#' @description transform_sp_local_r4_to_local_proc_r4() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform sp local ready4 s4 to local process ready4 s4. Function argument x specifies the object to be updated. Argument import_chr_vec provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @param import_chr_vec PARAM_DESCRIPTION
#' @param raw_data_dir_chr Raw data directory (a character vector)
#' @param save_lgl Save (a logical vector)
#' @return NULL
#' @rdname transform_sp_local_r4_to_local_proc_r4
#' @export 

#' @keywords internal
transform_sp_local_r4_to_local_proc_r4 <- function (x, import_chr_vec, raw_data_dir_chr, save_lgl) 
{
    ready4_sp_local_proc(lup_tbs_r4 = x@lup_tbs_r4, import_chr_vec = import_chr_vec, 
        merge_with_chr_vec = x@merge_with_chr_vec, overwrite_lgl = x@overwrite_lgl, 
        raw_data_dir_chr = raw_data_dir_chr, save_lgl = save_lgl)
}
#' Transform tt polygon to
#' @description transform_tt_polygon_to_sf() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform tt polygon to simple features object. Function argument tt_polyline specifies the object to be updated. Argument mode_of_transport provides the object to be updated. The function is called for its side effects and does not return a value.
#' @param tt_polyline PARAM_DESCRIPTION
#' @param mode_of_transport PARAM_DESCRIPTION
#' @param travel_time_hours PARAM_DESCRIPTION
#' @param ... Additional arguments
#' @return NULL
#' @rdname transform_tt_polygon_to_sf
#' @export 
#' @importFrom googlePolylines decode
#' @importFrom purrr pluck
#' @importFrom sf st_polygon st_sfc st_sf
#' @keywords internal
transform_tt_polygon_to_sf <- function (tt_polyline, mode_of_transport, travel_time_hours, 
    crs) 
{
    test_g_polyline <- tt_polyline %>% as.character()
    googlePolylines::decode(test_g_polyline) %>% purrr::pluck(1) %>% 
        as.matrix() %>% list() %>% sf::st_polygon() %>% sf::st_sfc() %>% 
        data.frame(mode_of_transport = mode_of_transport, travel_time_hours = travel_time_hours, 
            .) %>% sf::st_sf(crs = crs)
}
