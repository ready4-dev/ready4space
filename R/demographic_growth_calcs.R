#' @title
#' Demographic growth calculations
#'
#' @description
#' This function creates a tibble......
#'
#' @family demographic modelling functions.
#'
#' @details
#'
#' @param age.sex.ref A .....
#'
#' @param reference.year A .....
#'
#' @param base.year A .....
#'
#' @param fup.year A .....
#'
#' @param poppreds.list A list .....
#'
#' @param demographic.data A .....
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{funs}}
#'  \code{\link[sf]{st_geometry}}
#'  \code{\link[purrr]{pluck}}
#' @rdname demographic_growth_calcs
#' @export
#' @importFrom dplyr select pull mutate_all funs
#' @importFrom sf st_set_geometry
#' @importFrom purrr pluck

demographic_growth_calcs<-function(age.sex.ref,
                                          reference.year,
                                          base.year,
                                          fup.year,
                                          poppreds.list,
                                          demographic.data
){
  growth.prop <- (reference.year-base.year)/(fup.year-base.year)
  base.yr.ref <- paste0("intersect_pop.",
                            age.sex.ref,
                            ".",
                            base.year
  )
  fup.yr.ref <- paste0("intersect_pop.",
                           age.sex.ref,
                           ".",
                           fup.year
  )
  base.yr.vector<-demographic.data %>%
    dplyr::select(!!base.yr.ref) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::pull()
  fup.yr.vector<-demographic.data %>%
    dplyr::select(!!fup.yr.ref) %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::pull()
  pop.preds.tib<-poppreds.list %>%
    purrr::pluck(age.sex.ref) %>%
    dplyr::mutate_all(dplyr::funs(
      (./100)*base.yr.vector*growth.prop+base.yr.vector
    )
    )
  return(pop.preds.tib)
}
