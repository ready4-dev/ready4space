#' Reshape pop projs
#' @description reshape_pop_projs() is a Reshape function that modifies the shape of a data object. Specifically, this function implements an algorithm to reshape pop projs. The function is called for its side effects and does not return a value.
#' @param x PARAM_DESCRIPTION
#' @return NULL
#' @rdname reshape_pop_projs
#' @export 
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate_if mutate_at
#' @importFrom stringr str_trim
#' @keywords internal
reshape_pop_projs <- function (x) 
{
    reshaped_tb <- t(x)
    colnames(reshaped_tb) <- paste0("Persons.", as.character(unlist(reshaped_tb[1, 
        ])))
    reshaped_tb <- reshaped_tb[-1, ]
    reshaped_tb <- cbind(tibble::tibble(year = row.names(reshaped_tb)), 
        reshaped_tb) %>% tibble::as_tibble() %>% dplyr::mutate_if(is.factor, 
        .funs = as.character) %>% dplyr::mutate_if(is.character, 
        .funs = stringr::str_trim)
    reshaped_tb %>% dplyr::mutate_at(.vars = names(reshaped_tb)[!names(reshaped_tb) %in% 
        reshaped_tb$year], .funs = as.numeric)
}
