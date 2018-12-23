#' @title
#' Create a simple features object of two intersecting areas.
#'
#' @description
#' This function:
#'   -
#'   -
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param profiled_sf A SF object corresponding to the geographic unit that is to be profiled.
#'
#' @param profiled_colref A variable name from the profiled_sf object that is the basis for
#' subsetting the object if not all areas from the object are to be used.
#'
#' @param profiled_rworef A value from the profiled_colref variable which, if specified, will
#' be used to identify a subset of the profiled_sf object.
#'
#' @param resolution_sf A SF object corresponding to the geographic unit that is resolution
#' at which the profiled_sf object will be profiled.
#'
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
#' @rdname spatial_intersect_profiled_resolution_units
#' @export
#' @importFrom dplyr select pull slice
#' @importFrom stringr str_which
#' @importFrom sf st_intersection

spatial_intersect_profiled_resolution_units<-function(profiled_sf,
                                                      profiled_colref = NA,
                                                      profiled_rowref = NA,
                                                      resolution_sf
){
  if(!is.na(profiled_colref)){
    if(!is.na(profiled_rowref)){
      firstbit<-profiled_sf %>%
        dplyr::select(!!profiled_colref)
      index.nbr<-firstbit %>%
        dplyr::pull(profiled_colref) %>%
        stringr::str_which(profiled_rowref)
      firstbit<-firstbit %>%
        dplyr::slice(index.nbr)
    }else
      firstbit<-profiled_sf %>%
        dplyr::select(!!profiled_colref)
    profiled_by_resolution <- sf::st_intersection(firstbit,
                                          resolution_sf)
  }else
    profiled_by_resolution <- sf::st_intersection(profiled_sf,
                                          resolution_sf)
  return(profiled_by_resolution)
}
