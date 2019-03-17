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
#' @param group_by_var A string specifying "SA1", "SA2" or "LGA" as the resolution of the
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
                                        group_by_var,
                                        group_by_lookup_tb,
                                        data_year){
  age_sex_var_name <- ready.data::data_get(data_lookup_tb = group_by_lookup_tb %>%
                                         dplyr::filter(year == data_year),
                                       lookup_variable = "resolution",
                                       lookup_reference = age_sex_pop_resolution,
                                       target_variable = "var_name",
                                       evaluate = FALSE)
  profiled_sf <- intersect_sfs_keep_counts(profiled_sf = profiled_sf,
                                           profiled_colref = profiled_colref,
                                           profiled_rowref = profiled_rowref,
                                           attribute_sf = sp_data_list[[age_sex_pop_resolution]],
                                           attribute_unit = age_sex_pop_resolution,
                                           data_type = "age_sex",
                                           data_year = data_year)
  tot_pop_sf <- NULL
  if(!is.null(tot_pop_resolution)){
    tot_pop_sf <- sp_data_list[[tot_pop_resolution]]
    duplicate_names <- names(tot_pop_sf)[names(tot_pop_sf) %in% names(profiled_sf)[names(profiled_sf)!="geometry"]]
    if(!identical(duplicate_names,character(0))){
      tot_pop_sf <- tot_pop_sf %>%
        dplyr::rename_at(dplyr::vars(dplyr::one_of(duplicate_names)),
                         dplyr::funs(paste0("dupl_",
                                            tot_pop_resolution,
                                            "_",
                                            .)))
    }
    profiled_sf <- intersect_sfs_keep_counts(profiled_sf = profiled_sf,
                                             profiled_colref = profiled_colref,
                                             profiled_rowref = profiled_rowref,
                                             attribute_sf = tot_pop_sf,
                                             attribute_unit = tot_pop_resolution,
                                             data_type = "tot_pop")
  }
  profiled_sf <- update_pop_count_by_areas(profiled_sf = profiled_sf,
                                           group_by_var = group_by_var,
                                           age_sex_var_name = age_sex_var_name,
                                           data_year = data_year,
                                           age_sex_pop_resolution = age_sex_pop_resolution,
                                           tot_pop_resolution = tot_pop_resolution)

  return(profiled_sf)
}


