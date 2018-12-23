#' @title
#' Demographic modelling inputs......
#'
#' @description
#' This function creates a tibble......
#'
#' @family demographic modelling functions.
#'
#' @details
#'
#' @param error.data A .....
#'
#' @param demographic.data A .....
#'
#' @param error.listref A .....
#'
#' @param demographic.data.namestub A .....
#'
#' @param demographic.data.col.ref A .....
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
#'  \code{\link[purrr]{pluck}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[dplyr]{select}}
#' @rdname demographic_inputs
#' @export
#' @importFrom purrr pluck
#' @importFrom rlang sym
#' @importFrom dplyr select

demographic_inputs<-function(error.data,
                                       demographic.data,
                                       error.listref,
                                       demographic.data.namestub,
                                       demographic.data.col.ref
){
  demographic.projection.errors.partial <- error.data %>%
    purrr::pluck(error.listref)
  demographic.data.comp.ref<-rlang::sym(paste0(demographic.data.namestub,
                                               demographic.data.col.ref)
  )
  demographic.data.partial<-demographic.data %>%
    dplyr::select(!!demographic.data.comp.ref)
  poppreds.tib <- demographic_growth_projs_slice(demographic.projection.errors.partial,
                                              demographic.data.partial,
                                              paste0(demographic.data.col.ref,".")
  )
  return(poppreds.tib)
}
