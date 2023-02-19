#' Union one travel time band across sites
#' @description union_one_travel_time_band_across_sites() is an Union function that unions objects. Specifically, this function implements an algorithm to union one travel time band across sites. The function is called for its side effects and does not return a value.
#' @param time_band_ref PARAM_DESCRIPTION
#' @param one_cluster_time_bands_ls PARAM_DESCRIPTION
#' @return NULL
#' @rdname union_one_travel_time_band_across_sites
#' @export 
#' @importFrom purrr map pluck reduce
#' @importFrom sf st_union
#' @importFrom dplyr select
union_one_travel_time_band_across_sites <- function (time_band_ref, one_cluster_time_bands_ls) 
{
    list_of_new_sfs <- purrr::map(one_cluster_time_bands_ls, 
        ~.x %>% purrr::pluck(time_band_ref))
    purrr::reduce(list_of_new_sfs, ~sf::st_union(.x, .y) %>% 
        dplyr::select(id, min, max, center, drive_times))
}
