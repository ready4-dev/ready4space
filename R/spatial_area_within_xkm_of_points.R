
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
                                            distance){
  distance_from_pts_sf <- sf::st_as_sf(point_locations,
                                       coords = c("long", "lat"),
                                       crs = 4326) %>%
    sf::st_transform(3577)
  distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf,
                                                dist = distance) %>%
    sf::st_union() %>%
    sf::st_intersection(land_sf %>%
                          sf::st_transform(3577)) %>%
    sf::st_transform(4283) %>%
    sf::st_sf()
  return(distance_from_pts_on_land_sf)

}

