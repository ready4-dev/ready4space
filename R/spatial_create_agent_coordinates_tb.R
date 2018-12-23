#' @title
#' Creates a tibble of coordinates for synthetic incident cases.
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
#' @param profiled_area_sf A SF object corresponding to the geographic unit that is to be profiled.
#'
#' @param disorder A String ...
#'
#' @param year A String ...
#'
#' @param case_type A String ...
#'
#' @param person_type A String ...
#'
#' @param resolution_unit A String ...
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
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[sf]{st_geometry}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{filter}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map2}}
#' @rdname spatial_create_agent_coordinates_tb
#' @export
#' @importFrom stringr str_sub
#' @importFrom sf st_set_geometry
#' @importFrom dplyr select pull filter
#' @importFrom rlang sym
#' @importFrom purrr map2_dfr

spatial_create_agent_coordinates_tb <- function(profiled_area_sf,
                                                disorder,
                                                year,
                                                case_type = "expected.incidence",
                                                person_type = "p",
                                                resolution_unit){
  unit_col_name <- paste0(resolution_unit,
                          "_MAIN",
                          stringr::str_sub(year,3,4))
  cases_col_name <- paste0("proj_",
                           disorder,
                           "_",
                           person_type,
                           "_",
                           year)
  profiled_area_df <- profiled_area_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::select(!!rlang::sym(unit_col_name),
                  !!rlang::sym(cases_col_name))
  agent_coordinates_tb <- purrr::map2_dfr(profiled_area_df %>% dplyr::select(!!unit_col_name) %>% dplyr::pull(),
                                          profiled_area_df %>% dplyr::select(!!rlang::sym(cases_col_name)) %>% dplyr::pull(),
                                          ~ spatial_cases_coordinates(profiled_sf = profiled_area_sf %>%
                                                                                 dplyr::filter(!!rlang::sym(unit_col_name)==.x),
                                                                               incident_cases = .y))
  return(agent_coordinates_tb)
}
