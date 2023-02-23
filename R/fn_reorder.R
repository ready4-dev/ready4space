#' Reorder clusters by distances
#' @description reorder_clusters_by_distances() is a Reorder function that reorders an object to conform to a pre-specified schema. Specifically, this function implements an algorithm to reorder clusters by distances. The function returns Clusters by distance (a list).
#' @param clusters_by_distance_ls Clusters by distance (a list)
#' @param distances_dbl Distances (a double vector)
#' @param index_val_1L_int Index value (an integer vector of length one)
#' @return Clusters by distance (a list)
#' @rdname reorder_clusters_by_distances
#' @export 
#' @importFrom purrr map pluck
#' @importFrom dplyr mutate
#' @keywords internal
reorder_clusters_by_distances <- function (clusters_by_distance_ls, distances_dbl, index_val_1L_int) 
{
    clusters_by_distance_ls <- purrr::map(1:length(distances_dbl), 
        ~clusters_by_distance_ls %>% purrr::pluck(.x) %>% purrr::pluck(index_val_1L_int) %>% 
            dplyr::mutate(distance_in_km_dbl = distances_dbl %>% 
                purrr::pluck(.x) %>% paste0(c(0, distances_dbl) %>% 
                purrr::pluck(.x), " to ", ., "km")))
    return(clusters_by_distance_ls)
}
