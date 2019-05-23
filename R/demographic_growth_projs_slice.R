#' @title demographic_growth_projs_slice
#' @description Creates a tibble......
#' @param demographic.projection.errors.partial PARAM_DESCRIPTION
#' @param demographic.data.partial PARAM_DESCRIPTION
#' @param namestub PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[dplyr]{bind}}
#' @rdname demographic_growth_projs_slice
#' @export
#' @importFrom purrr pmap
#' @importFrom stats setNames
#' @importFrom dplyr bind_cols
demographic_growth_projs_slice <- function(demographic.projection.errors.partial,
                                           demographic.data.partial,
                                           namestub){
  pop.pred=list(a=demographic.data.partial,
                b=rep(demographic.projection.errors.partial,
                      length(demographic.data.partial)))
  poppreds.tib<-purrr::pmap(pop.pred,
                            ~ ..1+..2) %>%
    stats::setNames(paste(rep(paste0(namestub,
                                     "5.yr.pop.pred."),
                              length(demographic.projection.errors.partial)),
                          c(1:length(demographic.projection.errors.partial)),sep="")) %>%
    dplyr::bind_cols()
  return(poppreds.tib)
}
