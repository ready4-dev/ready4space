#' Transform agent areas
#' @description transform_agent_areas() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform agent areas. The function returns Agent areas (a tibble).
#' @param agent_areas_tb Agent areas (a tibble)
#' @param area_var_nm_1L_chr Area variable name (a character vector of length one), Default: 'Suburb'
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param title_case_1L_lgl Title case (a logical vector of length one), Default: T
#' @return Agent areas (a tibble)
#' @rdname transform_agent_areas
#' @export 
#' @importFrom dplyr filter mutate
#' @importFrom rlang sym
#' @importFrom stringr str_to_title
#' @keywords internal
transform_agent_areas <- function (agent_areas_tb, area_var_nm_1L_chr = "Suburb", match_var_nm_1L_chr = character(0), 
    match_value_xx = NULL, title_case_1L_lgl = T) 
{
    agent_areas_tb <- agent_areas_tb %>% dplyr::filter(!is.na(!!rlang::sym(match_var_nm_1L_chr))) %>% 
        dplyr::filter(!!rlang::sym(match_var_nm_1L_chr) == match_value_xx)
    proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 
        2)))
    agent_areas_tb <- agent_areas_tb %>% dplyr::mutate(`:=`(!!rlang::sym(area_var_nm_1L_chr), 
        proper(!!rlang::sym(area_var_nm_1L_chr)))) %>% dplyr::mutate(`:=`(!!rlang::sym(area_var_nm_1L_chr), 
        stringr::str_to_title(!!rlang::sym(area_var_nm_1L_chr))))
    return(agent_areas_tb)
}
#' Transform circles to bands
#' @description transform_circles_to_bands() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform circles to bands. The function returns Bands (a list).
#' @param geomc_dist_circles_ls Geometric distance circles (a list)
#' @return Bands (a list)
#' @rdname transform_circles_to_bands
#' @export 
#' @importFrom purrr map pluck
#' @importFrom sf st_difference
#' @importFrom dplyr select
#' @importFrom stats setNames
#' @keywords internal
transform_circles_to_bands <- function (geomc_dist_circles_ls) 
{
    bands_ls <- purrr::map(1:(length(geomc_dist_circles_ls) - 
        1), ~sf::st_difference(geomc_dist_circles_ls %>% purrr::pluck(.x + 
        1), geomc_dist_circles_ls %>% purrr::pluck(.x)) %>% dplyr::select(distance_in_km_dbl)) %>% 
        stats::setNames(paste0("dist_", 2:(geomc_dist_circles_ls %>% 
            length()))) %>% append(list(dist_1 = geomc_dist_circles_ls %>% 
        purrr::pluck(1)), after = 0)
    return(bands_ls)
}
#' Transform multiple values
#' @description transform_multiple_vals() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform multiple values. The function is called for its side effects and does not return a value.
#' @param distribution_chr Distribution (a character vector)
#' @param dstr_param_1_dbl Distribution parameter 1 (a double vector)
#' @param dstr_param_2_dbl Distribution parameter 2 (a double vector)
#' @param dstr_param_3_dbl Distribution parameter 3 (a double vector)
#' @param transformation_chr Transformation (a character vector)
#' @return Transformed vas (a numeric)
#' @rdname transform_multiple_vals
#' @export 
#' @importFrom purrr map_dbl
#' @keywords internal
transform_multiple_vals <- function (distribution_chr, dstr_param_1_dbl, dstr_param_2_dbl, 
    dstr_param_3_dbl, transformation_chr) 
{
    tfd_vas_num <- purrr::map_dbl(1:length(distribution_chr), 
        ~transform_value(distribution_chr[.x], dstr_param_1_dbl[.x], 
            dstr_param_2_dbl[.x], dstr_param_3_dbl[.x], transformation_chr[.x]))
    return(tfd_vas_num)
}
#' Transform polyline to simple features object
#' @description transform_polyline_to_sf() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform polyline to simple features object. The function returns Geometry (a simple features object).
#' @param polyline_xx Polyline (an output object of multiple potential types)
#' @param crs_1L_dbl Coordinates reference system (a double vector of length one)
#' @param mode_of_transport_1L_chr Mode of transport (a character vector of length one)
#' @param travel_time_hours_1L_dbl Travel time hours (a double vector of length one)
#' @return Geometry (a simple features object)
#' @rdname transform_polyline_to_sf
#' @export 
#' @importFrom googlePolylines decode
#' @importFrom purrr pluck
#' @importFrom sf st_polygon st_sfc st_sf
#' @keywords internal
transform_polyline_to_sf <- function (polyline_xx, crs_1L_dbl, mode_of_transport_1L_chr, 
    travel_time_hours_1L_dbl) 
{
    test_g_polyline <- polyline_xx %>% as.character()
    geometry_sf <- googlePolylines::decode(test_g_polyline) %>% 
        purrr::pluck(1) %>% as.matrix() %>% list() %>% sf::st_polygon() %>% 
        sf::st_sfc() %>% data.frame(mode_of_transport_chr = mode_of_transport_1L_chr, 
        travel_time_hours_dbl = c, .) %>% sf::st_sf(crs = crs_1L_dbl)
    return(geometry_sf)
}
#' Transform simple features object list
#' @description transform_sf_ls() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform simple features object list. The function returns Transformed (a list of simple features objects).
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return Transformed (a list of simple features objects)
#' @rdname transform_sf_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr select
#' @keywords internal
transform_sf_ls <- function (sf_ls) 
{
    common_vars_chr <- make_common_sf_vars_ls(sf_ls)
    transformed_sf_ls <- purrr::map(sf_ls, ~.x %>% dplyr::select(common_vars_chr))
    return(transformed_sf_ls)
}
#' Transform suffix to prefix
#' @description transform_sfx_to_pfx() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform suffix to prefix. The function returns Data (a tibble).
#' @param data_tb Data (a tibble)
#' @param suffix_1L_chr Suffix (a character vector of length one)
#' @return Data (a tibble)
#' @rdname transform_sfx_to_pfx
#' @export 
#' @importFrom dplyr rename_at vars ends_with funs
#' @keywords internal
transform_sfx_to_pfx <- function (data_tb, suffix_1L_chr) 
{
    data_tb <- data_tb %>% dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix_1L_chr)), 
        dplyr::funs(paste0(suffix_1L_chr, "_", gsub(paste0("_", 
            suffix_1L_chr), "", .))))
    return(data_tb)
}
#' Transform shape of population preductions dataset
#' @description transform_shape_of_popl_predns_ds() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform shape of population preductions dataset. The function returns Reshaped (a tibble).
#' @param popl_predns_ds PARAM_DESCRIPTION
#' @param prefix_1L_chr Prefix (a character vector of length one), Default: 'Persons.'
#' @return Reshaped (a tibble)
#' @rdname transform_shape_of_popl_predns_ds
#' @export 
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if mutate_at
#' @importFrom stringr str_trim
#' @keywords internal
transform_shape_of_popl_predns_ds <- function (popl_predns_ds, prefix_1L_chr = "Persons.") 
{
    reshaped_tb <- t(popl_predns_ds)
    colnames(reshaped_tb) <- paste0(prefix_1L_chr, as.character(unlist(reshaped_tb[1, 
        ])))
    reshaped_tb <- reshaped_tb[-1, ]
    reshaped_tb <- cbind(tibble::tibble(year_chr = row.names(reshaped_tb)), 
        reshaped_tb) %>% tibble::as_tibble() %>% dplyr::mutate_if(is.factor, 
        .funs = as.character) %>% dplyr::mutate_if(is.character, 
        .funs = stringr::str_trim)
    reshaped_tb <- reshaped_tb %>% dplyr::mutate_at(.vars = names(reshaped_tb)[!names(reshaped_tb) %in% 
        reshaped_tb$year_chr], .funs = as.numeric)
    return(reshaped_tb)
}
#' Transform to simpler simple features object
#' @description transform_to_simpler_sf() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform to simpler simple features object. The function returns Polygons (a simple features object).
#' @param sf Simple features object (a simple features object)
#' @param crs_dbl Coordinates reference system (a double vector), Default: NULL
#' @return Polygons (a simple features object)
#' @rdname transform_to_simpler_sf
#' @export 
#' @importFrom sf st_crs st_geometry_type st_collection_extract st_transform
#' @importFrom dplyr filter select
#' @importFrom geojsonio geojson_json geojson_sf
#' @importFrom rmapshaper ms_simplify
#' @keywords internal
transform_to_simpler_sf <- function (sf, crs_dbl = NULL) 
{
    if (is.null(crs_dbl)) 
        crs_dbl <- sf::st_crs(sf)[[1]]
    polygons_sf <- sf %>% dplyr::filter(sf::st_geometry_type(.) %in% 
        c("POLYGON", "MULTIPOLYGON"))
    sf_other <- sf %>% dplyr::filter(!sf::st_geometry_type(.) %in% 
        c("POLYGON", "MULTIPOLYGON"))
    if (nrow(sf_other) != 0) {
        sf_other <- sf_other %>% sf::st_collection_extract()
        polygons_sf <- rbind(polygons_sf, sf_other)
    }
    polygons_sf_json <- geojsonio::geojson_json(polygons_sf, 
        geometry = "polygon", type = "auto")
    simple_poly_json <- rmapshaper::ms_simplify(polygons_sf_json)
    polygons_sf <- geojsonio::geojson_sf(simple_poly_json) %>% 
        dplyr::select(-rmapshaperid) %>% sf::st_transform(crs = crs_dbl)
    return(polygons_sf)
}
#' Transform value
#' @description transform_value() is a Transform function that edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered. Specifically, this function implements an algorithm to transform value. The function is called for its side effects and does not return a value.
#' @param distribution_1L_chr Distribution (a character vector of length one)
#' @param dstr_param_1_1L_dbl Distribution parameter 1 (a double vector of length one)
#' @param dstr_param_2_1L_dbl Distribution parameter 2 (a double vector of length one)
#' @param dstr_param_3_1L_dbl Distribution parameter 3 (a double vector of length one)
#' @param transformation_1L_chr Transformation (a character vector of length one)
#' @return Transformed value length one (a numeric)
#' @rdname transform_value
#' @export 
#' @keywords internal
transform_value <- function (distribution_1L_chr, dstr_param_1_1L_dbl, dstr_param_2_1L_dbl, 
    dstr_param_3_1L_dbl, transformation_1L_chr) 
{
    if (distribution_chr == "none") 
        x <- dstr_param_1_1L_dbl
    if (!is.na(transformation_1L_chr)) 
        x <- eval(parse(text = transformation_1L_chr))
    tfd_val_1L_num <- x
    return(tfd_val_1L_num)
}
