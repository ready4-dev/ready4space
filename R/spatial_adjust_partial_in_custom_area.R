#' @title
#' Adjust partially included sub-unit of custom area of interest.
#'
#' @description
#' This function:
#'  - retrieves spatial information about the LGA which is partially contained within the
#'    custom area of interest;
#'  - identifies SA1s included in that LGA that are not part of custom area of interest and
#'    appends population attribute data to those SA1s;
#'  - identifies how much area and how much population is represented by the portion of the
#'    partially contained LGA that is outside that LGA;
#'  - creates an object representing that part of the partially contained LGA that lies within
#'    the custom area of interest; and
#'  - updates the area and population counts for this new object to represent only the proportion
#'    included in the custom area of interest.
#'
#' @family spatial functions.
#'
#' @details Need to generalise. Currently assumes LGA as main unit and SA1 as minor unit.
#'
#' @param main_unit_sf A simple features object.
#'
#' @param excluded_unit_sf A simple features object comprised of SA1s.
#'
#' @param excluded_refs A .....
#'
#' @param filter_one A .....
#'
#' @param filter_two A .....
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
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}}
#'  \code{\link[sf]{st_geometry}},\code{\link[sf]{geos_binary_ops}},\code{\link[sf]{geos_combine}}
#' @rdname spatial_adjust_partial_in_custom_area
#' @export
#' @importFrom dplyr filter summarise select mutate
#' @importFrom sf st_set_geometry st_difference st_union

spatial_adjust_partial_in_custom_area<-function(main_unit_sf,
                                                excluded_unit_sf,
                                                excluded_refs,
                                                filter_one,
                                                filter_two){
  one_main_unit <- main_unit_sf %>%
    dplyr::filter(STE_NAME16 == filter_one) %>%
    dplyr::filter(LGA_NAME16 %in% filter_two)
  custom_excluded_sa1s <- excluded_unit_sf %>%
    dplyr::filter(SA1_MAIN16 %in%
                    excluded_refs)
  custom_excluded_sa1s<-merge(custom_excluded_sa1s,
                                    pop_growth_by_sa1_2011_2016,
                                    by.x = c("SA1_7DIG16"),
                                    by.y = c("SA1"),
                                    all = FALSE)
  excluded.counts <- custom_excluded_sa1s %>%
    dplyr::summarise(excluded.area = sum(AREASQKM16),
                     excluded.pop = sum(X2011)) %>%
    sf::st_set_geometry(NULL)
  custom_one_main_unit_intersect<-sf::st_difference(one_main_unit,
                                                    sf::st_union(custom_excluded_sa1s %>%
                                                                   dplyr::select(-c(AREASQKM16,
                                                                                    STE_NAME16))))
  custom_one_main_unit_intersect <- custom_one_main_unit_intersect %>%
    dplyr::mutate(custom.area = AREASQKM16 - excluded.counts[1,1],
                  custom.2011.pop = total.pop.2011 - excluded.counts[1,2]) %>%
    dplyr::mutate(custom.prop.area = custom.area / AREASQKM16,
           custom.prop.pop = custom.2011.pop / total.pop.2011)
  return(custom_one_main_unit_intersect)
}
