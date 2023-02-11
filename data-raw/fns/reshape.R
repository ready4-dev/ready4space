reshape_pop_projs <- function(x){
  reshaped_tb <- t(x)
  colnames(reshaped_tb) <- paste0("Persons.",
                                  as.character(unlist(reshaped_tb[1,])))
  reshaped_tb <- reshaped_tb[-1,]
  reshaped_tb<- cbind(tibble::tibble(year_chr = row.names(reshaped_tb)),
                      reshaped_tb) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_if(is.factor,
                     .funs = as.character) %>%
    dplyr::mutate_if(is.character,
                     .funs = stringr::str_trim)
  reshaped_tb %>%
    dplyr::mutate_at(.vars = names(reshaped_tb)[!names(reshaped_tb) %in% reshaped_tb$year_chr],
                     .funs = as.numeric)
}
