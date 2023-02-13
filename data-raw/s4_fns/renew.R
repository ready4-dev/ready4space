renew_VicinityArguments <- function(x, # "makeProcessed_r4" #write_fls_from_local_imp
                                     raw_fls_dir_1L_chr,
                                     write_1L_lgl){
  updated_x_VicinityArguments <- x %>%
    renewSlot("write_1L_lgl",
              write_1L_lgl) %>%
    renewSlot("raw_fls_dir_1L_chr",
              raw_fls_dir_1L_chr)
  return(updated_x_VicinityArguments)
}

