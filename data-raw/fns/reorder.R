reorder_distance_list_by_cluster <- function(index_val_1L_int,
                                             clusters_by_distance_list,
                                             distances_vec){
  purrr::map(1:length(distances_vec),
             ~ clusters_by_distance_list %>%
               purrr::pluck(.x) %>%
               purrr::pluck(index_val_1L_int) %>%
               dplyr::mutate(distance_in_km_dbl = distances_vec %>%
                               purrr::pluck(.x) %>%
                               paste0(c(0,distances_vec)%>%
                                        purrr::pluck(.x),
                                      " to ",
                                      .,
                                      "km")))
}
