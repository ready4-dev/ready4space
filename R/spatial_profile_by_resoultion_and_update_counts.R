#' @title
#' Create a simple features object of two intersecting areas and adjust population counts by
#' fraction of spatial unit included in a profiled area.
#'
#' @description
#' This function:
#'  -
#'  -
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param resolution_sa1s_sf A simple features object comprised of SA1s
#'
#' @param resolution_sa2s_sf A simple features object comprised of SA2s.
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
#' @param return_resolution A string specifying "SA1", "SA2" or "LGA" as the resolution of the
#' returned object,
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
#' @rdname spatial_profile_by_resolution_and_update_counts
#' @export

spatial_profile_by_resolution_and_update_counts <- function(profiled_sf,
                                                            profiled_colref = NA,
                                                            profiled_rowref = NA,
                                                            resolution_sf,
                                                            resolution_sa1s_sf,
                                                            resolution_sa2s_sf,
                                                            return_resolution){

  profiled_object <- spatial_intersect_profiled_resolution_units(profiled_sf = profiled_sf,
                                                                              profiled_colref = profiled_colref,
                                                                              profiled_rowref = profiled_rowref,
                                                                              resolution_sf = resolution_sf
  )
  if(!"SA2_MAIN16" %in% names(profiled_object)){
    profiled_object <-  spatial_intersect_profiled_resolution_units(profiled_sf = profiled_object,
                                                                                resolution_sf = resolution_sa2s_sf
    )
  }
  profiled_object <- spatial_adjust_population_by_included_fraction(resolution_sa1s_sf = resolution_sa1s_sf,
                                                                                profiled_by_sa2_sf = profiled_object,
                                                                                return_resolution = return_resolution)
  return(profiled_object)
}
