#' Order tibble method applied toeadyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import..
#' @description order_tb.vicinity_raw() is an Order Tibble method that orders a tibble. This method is implemented for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.. The function is called for its side effects and does not return a value.
#' @param x An instance of Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @return NULL
#' @rdname order_tb-methods
#' @export 
#' @importFrom dplyr filter select mutate pull
#' @importFrom purrr map map2 reduce
order_tb.vicinity_raw <- function (x) 
{
    not_to_be_ordered_tb <- x %>% dplyr::filter(is.na(uid))
    x <- x %>% dplyr::filter(!is.na(uid))
    ordering_tb <- x %>% dplyr::select(name, uid, add_boundaries) %>% 
        dplyr::mutate(preceeded_by = purrr::map(add_boundaries, 
            ~unlist(.x)[unlist(.x) %in% uid])) %>% dplyr::mutate(sequence = purrr::map2(preceeded_by, 
        uid, ~c(.x, .y)))
    if (nrow(x) > 0) {
        ordering_vec <- purrr::reduce(ordering_tb %>% dplyr::pull(sequence), 
            ~append(.x, .y[!.y %in% .x]))
        x <- x[match(ordering_vec, x$uid), ]
    }
    bind_rows(x, not_to_be_ordered_tb)
}
#' @rdname order_tb-methods
#' @aliases order_tb,vicinity_raw-method
methods::setMethod("order_tb", "vicinity_raw", order_tb.vicinity_raw)
