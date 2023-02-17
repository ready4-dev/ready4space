# order_tb.vicinity_raw <- function(x){ #Now renew
#   not_to_be_ordered_tb <- x %>% dplyr::filter(is.na(uid))
#   x <- x %>% dplyr::filter(!is.na(uid))
#   ordering_tb <- x %>%
#     dplyr::select(name,uid,add_boundaries) %>%
#     dplyr::mutate(preceeded_by = purrr::map(add_boundaries,
#                                             ~ unlist(.x)[unlist(.x) %in% uid])) %>%
#     dplyr::mutate(sequence = purrr::map2(preceeded_by,
#                                          uid,
#                                          ~ c(.x,.y)))
#   if(nrow(x) > 0){
#     ordering_vec <- purrr::reduce(ordering_tb %>%
#                                     dplyr::pull(sequence),
#                                   ~ append(.x,.y[!.y %in% .x]))
#
#     x <- x[match(ordering_vec, x$uid),]
#   }
#   bind_rows(x,not_to_be_ordered_tb)
# }
