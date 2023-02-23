#' Reorder distance list by cluster
#' @description reorder_clusters_by_distances() is a Reorder function that changes ordering of an object. Specifically, this function implements an algorithm to reorder distance list by cluster. The function is called for its side effects and does not return a value.
#' @param index_val_1L_int PARAM_DESCRIPTION
#' @param clusters_by_distance_ls PARAM_DESCRIPTION
#' @param distances_dbl PARAM_DESCRIPTION
#' @return NULL
#' @rdname reorder_clusters_by_distances
#' @export 
#' @importFrom purrr map pluck
#' @importFrom dplyr mutate
reorder_clusters_by_distances <- function (index_val_1L_int, clusters_by_distance_ls, distances_dbl) 
{
    purrr::map(1:length(distances_dbl), ~clusters_by_distance_ls %>% 
        purrr::pluck(.x) %>% purrr::pluck(index_val_1L_int) %>% dplyr::mutate(distance_km = distances_dbl %>% 
        purrr::pluck(.x) %>% paste0(c(0, distances_dbl) %>% purrr::pluck(.x), 
        " to ", ., "km")))
}
