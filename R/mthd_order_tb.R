#' @title order_tb.ready4_sp_import_lup
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}},\code{\link[purrr]{reduce}}
#' @rdname order_tb.ready4_sp_import_lup
#' @export
#' @importFrom dplyr select mutate pull
#' @importFrom purrr map map2 reduce
#' @import ready4class
order_tb.ready4_sp_import_lup <- function(x){
  not_to_be_ordered_tb <- x %>% dplyr::filter(is.na(uid))
  x <- x %>% dplyr::filter(!is.na(uid))
  ordering_tb <- x %>%
    dplyr::select(name,uid,add_boundaries) %>%
    dplyr::mutate(preceeded_by = purrr::map(add_boundaries,
                                            ~ unlist(.x)[unlist(.x) %in% uid])) %>%
    dplyr::mutate(sequence = purrr::map2(preceeded_by,
                                         uid,
                                         ~ c(.x,.y)))
  if(nrow(x) > 0){
    ordering_vec <- purrr::reduce(ordering_tb %>%
                                    dplyr::pull(sequence),
                                  ~ append(.x,.y[!.y %in% .x]))

    x <- x[match(ordering_vec, x$uid),]
  }
  bind_rows(x,not_to_be_ordered_tb)
}
