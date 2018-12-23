#' @title
#' Demographic predictions list
#'
#' @description
#' This function creates a list......
#'
#' @family demographic modelling functions.
#'
#' @details
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
#' A list.
#'
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[stringr]{str_subset}}
#'  \code{\link[stats]{setNames}}
#' @rdname demographic_growth_list_single
#' @export
#' @importFrom purrr map
#' @importFrom stringr str_subset
#' @importFrom stats setNames
demographic_growth_list_single<-function(reference.year,
                                base.year,
                                fup.year,
                                poppreds.list,
                                demographic.data){
  new.pop.preds.list<-purrr::map(names(poppreds.list) %>%
                                  stringr::str_subset("15.19|20.24"),
                                ~ demographic_growth_calcs(age.sex.ref = .,
                                                                              reference.year = reference.year,
                                                                              base.year = base.year,
                                                                              fup.year = fup.year,
                                                                              poppreds.list = poppreds.list,
                                                                              demographic.data = demographic.data)
  ) %>%
    stats::setNames(names(poppreds.list) %>%
                     stringr::str_subset("15.19|20.24")
    )
  return(new.pop.preds.list)
}
