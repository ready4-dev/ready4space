#' intersect_sfs_update_counts
#' Create a simple features object of two intersecting areas and adjust population counts by
#' fraction of spatial unit included in a profiled area.
#' @param profiled_sf PARAM_DESCRIPTION
#' @param profiled_colref PARAM_DESCRIPTION, Default: NA
#' @param profiled_rowref PARAM_DESCRIPTION, Default: NA
#' @param sp_data_list PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param age_sex_counts_grouped_by PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}}
#' @rdname intersect_sfs_update_counts
#' @export
#' @importFrom dplyr rename_at vars one_of funs
intersect_sfs_update_counts <- function(profiled_sf,
                                        profiled_colref = NA,
                                        profiled_rowref = NA,
                                        sp_data_list,
                                        tot_pop_resolution,
                                        age_sex_pop_resolution,
                                        group_by_var,
                                        age_sex_counts_grouped_by,
                                        data_year){
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
                                           age_sex_var_name = age_sex_counts_grouped_by,
                                           data_year = data_year,
                                           age_sex_pop_resolution = age_sex_pop_resolution,
                                           tot_pop_resolution = tot_pop_resolution)

  return(profiled_sf)
}


