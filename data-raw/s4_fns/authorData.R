authorData_VicinityLocalRaw <- function(x, #"author" #write_raw_data_from_sp_local_raw ### Local
                                     return_r4_1L_lgl = F){
  vicinity_raw_r3 <- x@a_VicinityLookup@vicinity_raw_r3
  ready4use::assert_single_row_tb(vicinity_raw_r3)
  raw_format_sp_dir <- write_raw_format_dir(data_type_chr = vicinity_raw_r3$data_type_chr,
                                            raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr)
  imports_chr <- manufacture(x@a_VicinityLookup,#make_imports_chr
                            type_1L_chr = vicinity_raw_r3$data_type_chr,
                            what_1L_chr = "imports")
  write_1L_lgl <- author(x = vicinity_raw_r3,
                         match_vals_xx = imports_chr,
                         path_1L_chr = raw_format_sp_dir,#dir_1L_chr
                         overwrite_1L_lgl = x@overwrite_1L_lgl)
  if(return_r4_1L_lgl){
    return_xx <- metamorphose(x,
                              imports_chr = imports_chr,
                              raw_fls_dir_1L_chr = raw_format_sp_dir,
                              write_1L_lgl = write_1L_lgl)
  }
  return(return_xx)
}
