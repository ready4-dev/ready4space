#' @title
#' Demographic growth
#'
#' @description
#' This function creates a list......
#'
#' @family demographic modelling functions.
#'
#' @details
#'
#' @param demographic.projection.errors A .....
#'
#' @param demographic.data A .....
#'
#'
#' @return
#' A list.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_geometry}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_detect}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{keep}},\code{\link[purrr]{flatten}},\code{\link[purrr]{map2}}
#'  \code{\link[stats]{setNames}}
#' @rdname demographic_growth_list
#' @export
#' @importFrom sf st_set_geometry
#' @importFrom dplyr select starts_with
#' @importFrom stringr str_replace_all str_detect
#' @importFrom purrr map keep flatten map2
#' @importFrom stats setNames
demographic_growth_list<-function(demographic.projection.errors,
                                       demographic.data
){
  demographic.data<-demographic.data %>%
    sf::st_set_geometry(NULL)
  demographic.data<-demographic.data %>%
    dplyr::select(dplyr::starts_with("growth.pc.5y.2021")
    )
  allowable.age.sex.combos <- names(demographic.data) %>%
    stringr::str_replace_all("growth.pc.5y.2021.",
                             "")
  allowable.age.ranges<-allowable.age.sex.combos %>%
    stringr::str_replace_all("f.",
                             "") %>%
    stringr::str_replace_all("m.",
                             "") %>%
     unique()
  demographic.projection.errors<-purrr::map(allowable.age.ranges,
                                           ~ demographic.projection.errors %>%
                                             purrr::keep(names(.)%>%
                                                          stringr::str_detect(.x)
                                             )
  ) %>%
    purrr::flatten()
  poppreds.list<-purrr::map2(.x = sort(names(demographic.projection.errors)),
                      .y = allowable.age.sex.combos,
                      ~ demographic_inputs(error.data=demographic.projection.errors,
                                                                 demographic.data,
                                                                 error.listref = .x,
                                                                 demographic.data.namestub = "growth.pc.5y.2021.",
                                                                 demographic.data.col.ref = .y
                      )
  ) %>%
    stats::setNames(allowable.age.sex.combos)
  return(poppreds.list)
}
