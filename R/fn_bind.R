#' Bind isochrone bands
#' @description bind_isochrone_bands() is a Bind function that binds two objects together to create a composite object. Specifically, this function implements an algorithm to bind isochrone bands. The function returns Isochrone bands (a simple features object).
#' @param isochrone_bands_ls Isochrone bands (a list)
#' @param index_1L_int Index (an integer vector of length one)
#' @param travel_mode_1L_chr Travel mode (a character vector of length one)
#' @return Isochrone bands (a simple features object)
#' @rdname bind_isochrone_bands
#' @export 
#' @importFrom purrr map pluck reduce
#' @importFrom sf st_union
#' @importFrom dplyr select
#' @importFrom rlang sym
#' @keywords internal
bind_isochrone_bands <- function (isochrone_bands_ls, index_1L_int, travel_mode_1L_chr) 
{
    list_of_new_sfs <- purrr::map(isochrone_bands_ls, ~.x %>% 
        purrr::pluck(index_1L_int))
    isochrone_bands_sf <- purrr::reduce(list_of_new_sfs, ~sf::st_union(.x, 
        .y) %>% dplyr::select(id, isomin, isomax, !!rlang::sym(paste0(travel_mode_1L_chr, 
        "_times"))))
    return(isochrone_bands_sf)
}
