#' @title reshape_pop_projs
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}},\code{\link[tibble]{tibble}},\code{\link[tibble]{as_tibble}}
#'  \code{\link[dplyr]{mutate_all}}
#'  \code{\link[stringr]{str_trim}}
#' @rdname reshape_pop_projs
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if mutate_at
#' @importFrom stringr str_trim
reshape_pop_projs <- function(x){
  reshaped_tb <- t(x)
  colnames(reshaped_tb) <- paste0("Persons.",
                                  as.character(unlist(reshaped_tb[1,])))
  reshaped_tb <- reshaped_tb[-1,]
  reshaped_tb<- cbind(tibble::tibble(year = row.names(reshaped_tb)),
                      reshaped_tb) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.factor,
                     .funs = as.character) %>%
    dplyr::mutate_if(is.character,
                     .funs = stringr::str_trim)
  reshaped_tb %>%
    dplyr::mutate_at(.vars = names(reshaped_tb)[!names(reshaped_tb) %in% reshaped_tb$year],
                     .funs = as.numeric)
}
