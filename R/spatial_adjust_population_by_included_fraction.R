#' @title
#' Adjust population counts by fraction of spatial unit included in a profiled area.
#'
#' @description
#' This function:
#'  -
#'  -
#'
#' @family spatial functions.
#'
#' @details Need to review if removal of linestrings is appropriate.
#'
#' @param resolution_sa1s_sf A simple features object comprised of SA1s
#'
#' @param profiled_by_sa2_sf A simple features object comprised of SA2s.
#'
#' @param return_resolution A string specifying "SA1" or "SA2" as the resolution of the
#' returned object,
#'
#' @return
#' A simple features object.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{join}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{summarise_all}}
#'  \code{\link[sf]{st_geometry}},\code{\link[sf]{geos_measures}}
#' @rdname spatial_adjust_population_by_included_fraction
#' @export
#' @importFrom dplyr inner_join group_by summarise ungroup rename mutate filter select contains rename_at vars funs mutate_at summarise_at starts_with
#' @importFrom sf st_set_geometry st_area

spatial_adjust_population_by_included_fraction<-function(resolution_sa1s_sf,
                                                         profiled_by_sa2_sf,
                                                         return_resolution
){
  small_unit_sf <- dplyr::inner_join(resolution_sa1s_sf,
                                     resolution_sa1s_sf %>%
                                       sf::st_set_geometry(NULL) %>%
                                       dplyr::group_by(SA2_MAIN16) %>%
                                       dplyr::summarise(sa2_as_whole_all_age_2016 = sum(year_2016pr),
                                                        sa2_area_sqkm = sum(AREASQKM16)) %>%
                                       dplyr::ungroup())
  small_unit_sf <- small_unit_sf %>%
    dplyr::rename(sa1_area_sqkm = AREASQKM16) %>%
    dplyr::mutate(sa1_prop_of_sa2_pop = year_2016pr / sa2_as_whole_all_age_2016,
                  sa1_prop_of_sa2_area = sa1_area_sqkm / sa2_area_sqkm) %>%
    dplyr::mutate(sa1_prop_of_sa2_pop = replace(sa1_prop_of_sa2_pop,is.nan(sa1_prop_of_sa2_pop),0)) %>%
    dplyr::mutate(sa1_prop_of_sa2_area = pmin(sa1_prop_of_sa2_area,1))

  fine_grained_profiled_by_sa2_sf <- profiled_by_sa2_sf %>%
    dplyr::rename(sa2_area_sqkm_check = AREASQKM16) %>%
    dplyr::mutate(included_sa2_area_sqkm = sf::st_area(.) %>%
                    as.numeric()) %>%
    dplyr::mutate(included_sa2_area_sqkm = included_sa2_area_sqkm / 1000000) %>%
    dplyr::mutate(included_sa2_area_prop = included_sa2_area_sqkm /sa2_area_sqkm_check)  %>%
    dplyr::mutate(included_sa2_area_prop = pmin(included_sa2_area_sqkm,1))

  fine_grained_profiled_by_sa2_sf <- spatial_intersect_profiled_resolution_units(profiled_sf = fine_grained_profiled_by_sa2_sf,
                                                                                             profiled_colref  = NA,
                                                                                             profiled_rowref = NA,
                                                                                             resolution_sf = small_unit_sf)

  fine_grained_profiled_by_sa2_sf <- fine_grained_profiled_by_sa2_sf %>%
    dplyr::filter(SA2_NAME16==SA2_NAME16.1) # Removes linestring geometries. Could also use st_geometry_type()
  fine_grained_profiled_by_sa2_sf <- fine_grained_profiled_by_sa2_sf %>%
    dplyr::mutate(included_sa1_area_sqkm = sf::st_area(.) %>%
                    as.numeric()) %>%
    dplyr::mutate(included_sa1_area_sqkm = included_sa1_area_sqkm / 1000000) %>%
    dplyr::mutate(included_sa1_area_prop = included_sa1_area_sqkm /sa1_area_sqkm)
  fine_grained_profiled_by_sa2_sf <- fine_grained_profiled_by_sa2_sf %>%
    dplyr::select(SA1_7DIG16,
                  SA1_MAIN16,
                  SA2_MAIN16,
                  SA2_NAME16,
                  SA3_CODE16,
                  SA3_NAME16,
                  SA4_CODE16,
                  SA4_NAME16,
                  GCC_CODE16,
                  GCC_NAME16,
                  STE_CODE16,
                  STE_NAME16,
                  sa1_area_sqkm,
                  sa2_area_sqkm,
                  sa1_prop_of_sa2_area,
                  included_sa1_area_sqkm,
                  included_sa1_area_prop,
                  included_sa2_area_sqkm,
                  included_sa2_area_prop,
                  year_2011,
                  year_2012pr,
                  year_2013pr,
                  year_2014pr,
                  year_2015pr,
                  year_2016pr,
                  sa2_as_whole_all_age_2016,
                  sa1_prop_of_sa2_pop,
                  dplyr::contains("Female"),
                  dplyr::contains("Male"),
                  dplyr::contains("total")
    ) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("Female"),
                                 dplyr::contains("Male"),
                                 dplyr::contains("total")),
                     dplyr::funs(paste0("sa2_as_whole_",
                                              gsub("_sa2_as_whole","",.))))
  fine_grained_profiled_by_sa2_sf <- fine_grained_profiled_by_sa2_sf %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("Female"),
                                 dplyr::contains("Male"),
                                 dplyr::contains("total")),
                     dplyr::funs(sa1_included =.*included_sa1_area_prop*sa1_prop_of_sa2_pop))  %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("_sa1_included")),
                     dplyr::funs(paste0("sa1_included_",
                                              gsub("_sa1_included","",.))%>%
                                   gsub("sa2_as_whole_","",.)))
  if(return_resolution=="SA1"){
    resolution_sf <- fine_grained_profiled_by_sa2_sf
  }else{
    resolution_sf <- profiled_by_sa2_sf
  }
  fine_grained_profiled_by_sa2_sf <- fine_grained_profiled_by_sa2_sf  %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(SA2_MAIN16) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with("sa1_included_")),
                        dplyr::funs(sa2_included = sum(.))) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("sa2_included")),
                     dplyr::funs(paste0("sa2_included_",
                                              gsub("_sa2_included","",.)) %>%
                                   gsub("sa1_included_","",.)))

  if(return_resolution=="SA1"){
    fine_grained_profiled_by_sa2_sf <- fine_grained_profiled_by_sa2_sf %>%
      dplyr::ungroup()
  }

  resolution_sf <-  dplyr::inner_join(resolution_sf,
                                      fine_grained_profiled_by_sa2_sf
  )

  if(return_resolution=="LGA"){
    resolution_sf <- resolution_sf %>%
      dplyr::group_by("LGA_NAME16") %>%
      dplyr::summarise_at(dplyr::vars(dplyr::starts_with("sa2_included_")),
                          dplyr::funs(lga_included = sum(.))) %>%
      dplyr::rename_at(dplyr::vars(dplyr::contains("lga_included")),
                       dplyr::funs(paste0("lga_included_",
                                                gsub("_lga_included","",.)) %>%
                                     gsub("sa2_included_","",.)))
  }
  return(resolution_sf)
}
