#' @title Calculate area within a specified distance of a cluster of points.
#' @description FUNCTION_DESCRIPTION
#' @param distance_km PARAM_DESCRIPTION
#' @param clusters_vec PARAM_DESCRIPTION
#' @param clusters_tbs_list PARAM_DESCRIPTION
#' @param land_boundary_sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{pluck}}
#'  \code{\link[stats]{setNames}}
#' @rdname spatial_area_within_xkm_of_cluster
#' @export
#' @importFrom purrr map pluck
#' @importFrom stats setNames

spatial_area_within_xkm_of_cluster <- function(distance_km,
                                               clusters_vec,
                                               clusters_tbs_list,
                                               land_boundary_sf){
  purrr::map(1:length(clusters_vec),
             ~ spatial_area_within_xkm_of_points(point_locations = clusters_tbs_list %>%
                                                   purrr::pluck(.x),
                                                 land_sf = land_boundary_sf,
                                                 distance = distance_km *1000)) %>%
    stats::setNames(., clusters_tbs_list %>% names())

}

#' @describeIn spatial_area_within_xkm_of_cluster Calculates the .....
#' @param look_up_ref PARAM_DESCRIPTION
#' @param clusters_by_distance_list PARAM_DESCRIPTION
#' @param distances_vecclusters_tbs_list PARAM_DESCRIPTION

reorder_distance_list_by_cluster <- function(look_up_ref,
                                             clusters_by_distance_list,
                                             distances_vec){
  purrr::map(1:length(distances_vec),
             ~ clusters_by_distance_list %>%
               purrr::pluck(.x) %>%
               purrr::pluck(look_up_ref) %>%
               dplyr::mutate(distance_km = distances_vec %>%
                               purrr::pluck(.x) %>%
                               paste0(c(0,distances_vec)%>%
                                        purrr::pluck(.x),
                                      " to ",
                                      .,
                                      "km")))
}

#' @describeIn spatial_area_within_xkm_of_cluster Calculates the .....
#' @param geom_distance_circle_sfs_list PARAM_DESCRIPTION
geom_distance_circles_to_bands <- function(geom_distance_circle_sfs_list){
  purrr::map(1:(length(geom_distance_circle_sfs_list)-1),
             ~ sf::st_difference(geom_distance_circle_sfs_list %>%
                                   purrr::pluck(.x+1),
                                 geom_distance_circle_sfs_list %>%
                                   purrr::pluck(.x)) %>%
               dplyr::select(distance_km)) %>%
    stats::setNames(paste0("dist_"
                           ,2:(geom_distance_circle_sfs_list  %>%
                                 base::length())))  %>%
    purrr::prepend(list(dist_1 = geom_distance_circle_sfs_list %>%
                          purrr::pluck(1)))

}
