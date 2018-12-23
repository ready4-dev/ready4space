#' @title
#' Summarise the 15-24 populaion by region for a given year.
#'
#' @description
#' This function creates a summary tibble of the female and male 15-24 year old
#' populations for each inputted region.
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param data A tibble with population data summarised by age, sex and region.
#'
#' @param first_colref A positive integer.
#'
#'
#' @return
#' A tibble.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[rlang]{sym}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  \code{\link[sf]{st_geometry}}
#' @rdname pop_sum_15.24
#' @export
#' @importFrom rlang sym
#' @importFrom dplyr mutate select contains
#' @importFrom sf st_set_geometry
pop_sum_15.24<-function(data, year){
  intersect_pop_female.15.24 <- paste0("intersect_pop.f.15.24.",
                                             year)
  int.f.15.19<-rlang::sym(paste0("intersect_pop.f.15.19.",
                                       year))
  int.f.20.24<-rlang::sym(paste0("intersect_pop.f.20.24.",
                                       year))
  intersect_pop_male.15.24<-paste0("intersect_pop.m.15.24.",
                                         year)
  int.m.15.19<-rlang::sym(paste0("intersect_pop.m.15.19.",
                                       year))
  int.m.20.24<-rlang::sym(paste0("intersect_pop.m.20.24.",
                                       year))
  output_tib<-data %>%
    dplyr::mutate(!!intersect_pop_female.15.24 := (!!int.f.15.19)+(!!int.f.20.24),
                  !!intersect_pop_male.15.24 := (!!int.m.15.19)+(!!int.m.20.24)
    ) %>%
    dplyr::select(#LGA_CODE16,
      dplyr::contains("intersect_pop.f.15.24"),
      dplyr::contains("intersect_pop.m.15.24")
    ) %>%
    sf::st_set_geometry(NULL)
  return(output_tib)
}
