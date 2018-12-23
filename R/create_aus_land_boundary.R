#' @title create_australia_land_boundary
#' @description Create a boundary SF of Australia
#' @param aus_boundary_sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}}
#'  \code{\link[sf]{geos_combine}}
#' @rdname create_australia_land_boundary
#' @export
#' @importFrom dplyr group_by summarise
#' @importFrom sf st_union
create_australia_land_boundary <- function(aus_boundary_sf){
  new_boundary_st_sf <- aus_boundary_sf %>%
    dplyr::group_by(FIRST_STE1) %>%
    dplyr::summarise(AREASQ = sum(SUM_AREASQ)) %>%
    sf::st_union()
  return(new_boundary_st_sf)
}
