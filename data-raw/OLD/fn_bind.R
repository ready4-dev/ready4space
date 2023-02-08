#' Bind simple features object rows function from web
#' @description bind_sf_rows_fn_from_web() is a Bind function that binds rows to data-frame type objects. Specifically, this function implements an algorithm to bind simple features object rows function from web. The function is called for its side effects and does not return a value.
#' @param ... Additional arguments
#' @return NA ()
#' @rdname bind_sf_rows_fn_from_web
#' @export 
#' @importFrom rlang dots_values
#' @importFrom sf st_sfc st_set_geometry st_sf
#' @importFrom dplyr bind_rows
bind_sf_rows_fn_from_web <- function (...) 
{
    sf_list <- rlang::dots_values(...)[[1]]
    sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% 
        sf::st_sfc()
    df <- lapply(sf_list, function(sf) sf::st_set_geometry(sf, 
        NULL)) %>% dplyr::bind_rows()
    sf_appended <- sf::st_sf(data.frame(df, geom = sfg_list_column))
    return(sf_appended)
}
