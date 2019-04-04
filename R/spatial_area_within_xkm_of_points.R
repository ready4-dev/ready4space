
#' @title
#' Plots a profiled geographic unit based on distancw from service location.
#'
#' @description
#' This function:
#'   -
#'   -
#'
#' @family plot functions.
#'
#' @details
#'
#' @param land_sf A SF object ....
#'
#' @param point_locations A tibble...
#'
#' @param distance A double....
#'
#' @return
#' A simple features object.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_as_sf}},\code{\link[sf]{st_transform}},\code{\link[sf]{geos_unary}},\code{\link[sf]{geos_combine}},\code{\link[sf]{geos_binary_ops}},\code{\link[sf]{sf}}
#' @rdname spatial_area_within_xkm_of_points
#' @export
#' @importFrom sf st_as_sf st_transform st_buffer st_union st_intersection st_sf
spatial_area_within_xkm_of_points<-function(point_locations,
                                            land_sf,
                                            distance,
                                            crs_nbr){
  distance_from_pts_sf <- sf::st_as_sf(point_locations,
                                       coords = c("long", "lat"),
                                       crs = crs_nbr) %>% #4326)
    sf::st_transform(3577)
  distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf,
                                                dist = distance) %>%
    sf::st_union() %>%
    sf::st_intersection(land_sf %>%
                          sf::st_transform(3577)
                        ) %>% #3577
    sf::st_transform(crs_nbr) %>%
    sf::st_sf()
  return(distance_from_pts_on_land_sf)

}

#' gen_distance_based_bands
#' FUNCTION_DESCRIPTION
#' @param distance_km_outer PARAM_DESCRIPTION
#' @param nbr_distance_bands PARAM_DESCRIPTION
#' @param service_cluster_tb PARAM_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{arrange}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[stats]{setNames}}
#' @rdname gen_distance_based_bands
#' @export
#' @importFrom dplyr pull filter arrange
#' @importFrom purrr map
#' @importFrom stats setNames

gen_distance_based_bands <- function(distance_km_outer,
                                     nbr_distance_bands,
                                     service_cluster_tb,
                                     profiled_sf,
                                     crs_nbr){
  distances_vec <- seq(from = distance_km_outer/nbr_distance_bands,
                       to = distance_km_outer,
                       by = distance_km_outer/nbr_distance_bands)

  service_clusters_vec <- service_cluster_tb %>% dplyr::pull(cluster_name) %>% unique()
  #state_territories_vec <- service_cluster_tb %>% dplyr::pull(state_territory) %>% unique()
  ## 1.2.2 Get distance / travel time based boundaries
  service_clusters_tbs_list <- purrr::map(service_clusters_vec,
                                          ~ service_cluster_tb %>%
                                            dplyr::filter(cluster_name == .x)) %>%
    stats::setNames(service_clusters_vec)
  ##
  service_clusters_by_distance_list <- purrr::map(distances_vec,
                                                  ~ spatial_area_within_xkm_of_cluster(distance_km = .x,
                                                                                       clusters_vec = service_clusters_vec,
                                                                                       clusters_tbs_list = service_clusters_tbs_list,
                                                                                       land_boundary_sf = profiled_sf,
                                                                                       crs_nbr = crs_nbr)) %>%
    stats::setNames(., paste0("km_",
                              distances_vec,
                              "from_service"))
  ##
  geometric_distance_by_cluster_circles <- purrr::map(1:length(service_clusters_vec),
                                                      ~ reorder_distance_list_by_cluster(look_up_ref = .x,
                                                                                                       clusters_by_distance_list = service_clusters_by_distance_list,
                                                                                                       distances_vec = distances_vec)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names())
  ##
  geometric_distance_by_cluster_bands <- purrr::map(geometric_distance_by_cluster_circles,
                                                    ~ geom_distance_circles_to_bands(geom_distance_circle_sfs_list = .x)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names())
  ##
  geometric_distance_by_cluster_circles_merged_list <- purrr::map(geometric_distance_by_cluster_circles,
                                                                  ~ do.call(rbind,.x)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names()) %>%
    purrr::map(.,
               ~ .x %>% dplyr::arrange(desc(distance_km)))
  ##
  geometric_distance_by_cluster_bands_merged_list <- purrr::map(geometric_distance_by_cluster_bands,
                                                                ~ do.call(rbind,.x)) %>%
    stats::setNames(., service_clusters_tbs_list %>% names()) %>%
    purrr::map(.,
               ~ .x %>% dplyr::arrange(desc(distance_km)))
  return(geometric_distance_by_cluster_bands_merged_list)
}

