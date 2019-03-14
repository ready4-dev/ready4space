#' @title intersect_sfs_keep_counts
#' This function:
#'   -
#' @details
#'
#' @param profiled_sf A SF object corresponding to the geographic unit that is to be profiled.
#'
#' @param profiled_colref A variable name from the profiled_sf object that is the basis for
#' filtering the object if not all areas from the object are to be used.
#'
#' @param profiled_rowref A value from the profiled_colref variable which, if specified, will
#' be used to identify a subset of the profiled_sf object.
#'
#' @param attribute_sf A SF object corresponding to the geographic unit that is resolution
#' at which the profiled_sf object will be profiled.
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{slice}}
#'  \code{\link[stringr]{str_subset}}
#'  \code{\link[sf]{geos_binary_ops}}
#' @rdname intersect_sfs_keep_counts
#' @export
#' @importFrom dplyr select pull slice
#' @importFrom stringr str_which
#' @importFrom sf st_intersection

intersect_sfs_keep_counts <- function(profiled_sf,
                                      profiled_colref = NA,
                                      profiled_rowref = NA,
                                      attribute_sf,
                                      attribute_unit){
  if(!is.na(profiled_colref)){
    if(!is.na(profiled_rowref)){
      profiled_sf <- profiled_sf %>%
        dplyr::select(!!profiled_colref)
      index.nbr <- profiled_sf %>%
        dplyr::pull(profiled_colref) %>%
        stringr::str_which(profiled_rowref)
      profiled_sf <- profiled_sf %>%
        dplyr::slice(index.nbr)
    }else
      profiled_sf <- profiled_sf %>%
        dplyr::select(!!profiled_colref)
  }
  attribute_sf <- attribute_sf %>%
    dplyr::mutate(!!rlang::sym(paste0("area_whl_",attribute_unit)) := sf::st_area(.) %>% units::set_units(km^2))
  profiled_sf <- sf::st_intersection(profiled_sf,
                                     attribute_sf) %>%
    dplyr::mutate(geom_type = sf::st_geometry_type(.)) %>%
    dplyr::filter(geom_type %in% c("POLYGON","MULTIPOLYGON"))
  return(profiled_sf)
}
#' @describeIn intersect_sfs_keep_counts  Function to intesect two sfs and drop extra columns.
#' @export

intersect_sf_drop_cols <- function(main_sf,
                                   adjunct_sf){

  drop_names <- names(main_sf)[names(main_sf) %in% names(adjunct_sf)]
  drop_names <- drop_names[-stringr::str_which(drop_names,"geometry")]
  sf::st_intersection(main_sf,
                      adjunct_sf %>%
                        dplyr::select(-drop_names))
}
