save_raw.ready4_sp_import_lup <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                          required_data,
                                          destination_directory,
                                          overwrite_lgl = F){
  purrr::map_lgl(required_data,
                 ~ download_data(x = x,
                                 destination_directory = destination_directory,
                                 data_lookup_ref = .x,
                                 overwrite_lgl = overwrite_lgl))
}
