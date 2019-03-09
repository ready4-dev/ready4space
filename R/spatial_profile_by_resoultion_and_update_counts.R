#' intersect_sfs_update_counts
#' Create a simple features object of two intersecting areas and adjust population counts by
#' fraction of spatial unit included in a profiled area.
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

intersect_sfs_update_counts <- function(profiled_sf,
                                    profiled_colref = NA,
                                    profiled_rowref = NA,
                                    sp_data_list,
                                    tot_pop_resolution,
                                    age_sex_pop_resolution,
                                    return_resolution,
                                    var_name_lookup_tb){
  res_var_name <- ready.data::data_get(data_lookup_tb = var_name_lookup_tb,
                                       lookup_variable = "resolution",
                                       lookup_reference = return_resolution,
                                       target_variable = "var_name",
                                       evaluate = FALSE)
  profiled_object <- intersect_sfs_keep_counts(profiled_sf = profiled_sf,
                                               profiled_colref = profiled_colref,
                                               profiled_rowref = profiled_rowref,
                                               attributes_sf = sp_data_list[[return_resolution]])
  if(!res_var_name %in% names(profiled_object)){
    profiled_object <-  intersect_sfs_keep_counts(profiled_sf = profiled_object,
                                                  attributes_sf = sp_data_list[[age_sex_pop_resolution]])
  }
  profiled_object <- spatial_adjust_population_by_included_fraction(resolution_sa1s_sf = sp_data_list[[tot_pop_resolution]],
                                                                    profiled_by_sa2_sf = profiled_object,
                                                                    return_resolution = return_resolution)
  return(profiled_object)
}
