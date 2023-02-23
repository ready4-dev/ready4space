#' Get included years
#' @description get_included_yrs() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get included years. Function argument geometry_sf specifies the where to look for the required object. The function returns Years (a double vector).
#' @param geometry_sf Geometry (a simple features object)
#' @param pfx_1L_chr Prefix (a character vector of length one), Default: 'y2'
#' @return Years (a double vector)
#' @rdname get_included_yrs
#' @export 
#' @importFrom sf `st_geometry<-`
#' @importFrom dplyr select starts_with
#' @importFrom stringr str_sub
#' @keywords internal
get_included_yrs <- function (geometry_sf, pfx_1L_chr = "y2") 
{
    geometry_sf <- geometry_sf %>% sf::`st_geometry<-`(NULL)
    if (pfx_1L_chr != "") 
        geometry_sf <- geometry_sf %>% dplyr::select(dplyr::starts_with(pfx_1L_chr))
    years_dbl <- geometry_sf %>% names() %>% stringr::str_sub(start = 2, 
        end = 5) %>% unique() %>% as.numeric()
    return(years_dbl)
}
#' Get maximum or minimum year of simple features object
#' @description get_max_or_min_yr_of_sf() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get maximum or minimum year of simple features object. Function argument geometry_sf specifies the where to look for the required object. The function returns Year (a double vector of length one).
#' @param geometry_sf Geometry (a simple features object)
#' @param max_1L_lgl Maximum (a logical vector of length one), Default: T
#' @param pfx_1L_chr Prefix (a character vector of length one), Default: 'y2'
#' @return Year (a double vector of length one)
#' @rdname get_max_or_min_yr_of_sf
#' @export 
#' @keywords internal
get_max_or_min_yr_of_sf <- function (geometry_sf, max_1L_lgl = T, pfx_1L_chr = "y2") 
{
    years_dbl <- get_included_yrs(geometry_sf, pfx_1L_chr = pfx_1L_chr)
    if (max_1L_lgl) 
        year_1L_dbl <- max(years_dbl)
    else year_1L_dbl <- min(years_dbl)
    return(year_1L_dbl)
}
#' Get name from path character vector
#' @description get_name_from_path_chr() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get name from path character vector. Function argument path_1L_chr specifies the where to look for the required object. The function returns Name (a character vector of length one).
#' @param path_1L_chr Path (a character vector of length one)
#' @param with_ext_1L_lgl With extension (a logical vector of length one), Default: TRUE
#' @return Name (a character vector of length one)
#' @rdname get_name_from_path_chr
#' @export 
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
#' @keywords internal
get_name_from_path_chr <- function (path_1L_chr, with_ext_1L_lgl = TRUE) 
{
    if (with_ext_1L_lgl) {
        name_1L_chr <- stringr::str_sub(path_1L_chr, start = stringi::stri_locate_last_regex(path_1L_chr, 
            "/")[, 2] %>% as.vector() + 1)
    }
    else {
        name_1L_chr <- stringr::str_sub(path_1L_chr, start = stringi::stri_locate_last_regex(path_1L_chr, 
            "/")[, 2] %>% as.vector() + 1, end = stringi::stri_locate_last_regex(path_1L_chr, 
            "\\.")[, 2] %>% as.vector() - 1)
    }
    return(name_1L_chr)
}
#' Get set difference longitude lat simple features object
#' @description get_set_diff_lng_lat_sf() is a Get function that retrieves a pre-existing data object from memory, local file system or online repository. Specifically, this function implements an algorithm to get set difference longitude lat simple features object. Function argument geometry_sf specifies the where to look for the required object. The function returns Difference (a simple features object).
#' @param geometry_sf Geometry (a simple features object)
#' @param cut_sf Cut (a simple features object)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param min_polygon_area_dbl Minimum polygon area (a double vector), Default: units::set_units(0.05, km^2)
#' @param validate_1L_lgl Validate (a logical vector of length one), Default: T
#' @return Difference (a simple features object)
#' @rdname get_set_diff_lng_lat_sf
#' @export 
#' @importFrom units set_units
#' @importFrom sf st_difference st_transform st_union st_cast st_area
#' @importFrom dplyr mutate n pull filter select
#' @importFrom purrr map map_dfr
#' @keywords internal
get_set_diff_lng_lat_sf <- function (geometry_sf, cut_sf, crs_nbr_dbl, min_polygon_area_dbl = units::set_units(0.05, 
    km^2), validate_1L_lgl = T) 
{
    new_sf <- sf::st_difference(geometry_sf %>% sf::st_transform(crs = crs_nbr_dbl[2]), 
        sf::st_union(cut_sf) %>% sf::st_transform(crs = crs_nbr_dbl[2])) %>% 
        sf::st_transform(crs = crs_nbr_dbl[1])
    if (validate_1L_lgl) 
        new_sf <- new_sf %>% make_valid_new_sf()
    new_sf <- new_sf %>% dplyr::mutate(feature_idx_int = 1:dplyr::n())
    new_ls <- purrr::map(dplyr::pull(new_sf, feature_idx_int), 
        ~new_sf %>% dplyr::filter(feature_idx_int == .x) %>% 
            sf::st_cast("POLYGON") %>% dplyr::mutate(new_area_dbl = sf::st_area(.)) %>% 
            dplyr::filter(new_area_dbl > min_polygon_area_dbl) %>% 
            sf::st_cast() %>% dplyr::select(-new_area_dbl, -feature_idx_int))
    if (length(new_ls) > 1) {
        difference_sf <- purrr::map_dfr(new_ls, ~.x)
    }
    else {
        difference_sf <- new_ls[[1]]
    }
    return(difference_sf)
}
