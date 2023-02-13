author.vicinity_raw <- function(x,
                                match_vals_xx,
                                dir_1L_chr,
                                overwrite_1L_lgl = F){
  files_written_lgl <- purrr::map_lgl(match_vals_xx,
                                      ~ authorData(x = x,
                                                   dir_1L_chr = dir_1L_chr,
                                                   data_match_value_xx = .x,
                                                   overwrite_1L_lgl = overwrite_1L_lgl))
  return(files_written_lgl)
}
