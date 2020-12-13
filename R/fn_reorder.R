#' Reorder distance list by cluster
#' @description reorder_distance_list_by_cluster() is a Reorder function that changes ordering of an object. Specifically, this function implements an algorithm to reorder distance list by cluster. The function is called for its side effects and does not return a value.
#' @param look_up_ref PARAM_DESCRIPTION
#' @param clusters_by_distance_list PARAM_DESCRIPTION
#' @param distances_vec PARAM_DESCRIPTION
#' @return NULL
#' @rdname reorder_distance_list_by_cluster
#' @export 
#' @importFrom purrr map pluck
#' @importFrom dplyr mutate
#' @keywords internal
reorder_distance_list_by_cluster <- function (look_up_ref, clusters_by_distance_list, distances_vec) 
{
    purrr::map(1:length(distances_vec), ~clusters_by_distance_list %>% 
        purrr::pluck(.x) %>% purrr::pluck(look_up_ref) %>% dplyr::mutate(distance_km = distances_vec %>% 
        purrr::pluck(.x) %>% paste0(c(0, distances_vec) %>% purrr::pluck(.x), 
        " to ", ., "km")))
}
