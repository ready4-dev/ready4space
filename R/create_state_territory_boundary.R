#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param aus_boundary_sf PARAM_DESCRIPTION
#' @param s_t PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}}
#' @rdname create_state_territory_boundary
#' @export
#' @importFrom dplyr filter group_by summarise

create_state_territory_boundary <- function(aus_boundary_sf,
                                            s_t){
  new_boundary_st_sf <- aus_boundary_sf %>%
    dplyr::filter(FIRST_STE1==(!!s_t)) %>%
    dplyr::group_by(FIRST_STE1) %>%
    dplyr::summarise(AREASQ = sum(SUM_AREASQ))
}
