#' Reshape pop projs
#' @description transform_shape_of_popl_predns_ds() is a Reshape function that modifies the shape of a data object. Specifically, this function implements an algorithm to reshape pop projs. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname transform_shape_of_popl_predns_ds
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if mutate_at
#' @importFrom stringr str_trim
transform_shape_of_popl_predns_ds <- function (x)
{
    reshaped_tb <- t(x)
    colnames(reshaped_tb) <- paste0("Persons.", as.character(unlist(reshaped_tb[1,
        ])))
    reshaped_tb <- reshaped_tb[-1, ]
    reshaped_tb <- cbind(tibble::tibble(year_chr = row.names(reshaped_tb)),
        reshaped_tb) %>% tibble::as_tibble() %>% dplyr::mutate_if(is.factor,
        .funs = as.character) %>% dplyr::mutate_if(is.character,
        .funs = stringr::str_trim)
    reshaped_tb %>% dplyr::mutate_at(.vars = names(reshaped_tb)[!names(reshaped_tb) %in%
        reshaped_tb$year_chr], .funs = as.numeric)
}
