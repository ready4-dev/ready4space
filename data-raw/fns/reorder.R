reorder_clusters_by_distances <- function(clusters_by_distance_ls,
                                             distances_dbl,
                                             index_val_1L_int){
  clusters_by_distance_ls <- purrr::map(1:length(distances_dbl),
             ~ clusters_by_distance_ls %>%
               purrr::pluck(.x) %>%
               purrr::pluck(index_val_1L_int) %>%
               dplyr::mutate(distance_in_km_dbl = distances_dbl %>%
                               purrr::pluck(.x) %>%
                               paste0(c(0,distances_dbl)%>%
                                        purrr::pluck(.x),
                                      " to ",
                                      .,
                                      "km")))
  return(clusters_by_distance_ls)
}
