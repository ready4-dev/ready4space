ratify.vicinity_processed <- function(x,
                                      data_items_chr = character(0),
                                      key_var_1L_chr = character(0),
                                      what_1L_chr = "population"){
  if(what_1L_chr == "population"){
   ratified_1L_lgl <- validate_popl_predns_incld(data_items_chr = data_items_chr,
                               data_lookup_tb = x,
                               key_var_1L_chr = key_var_1L_chr)
  }
  return(ratified_1L_lgl)
}
