manufacture.vicinity_raw <- function(x, ## write_fls_and_mk_sngl_row_data_lup
                                     merge_itms_chr,
                                     package_1L_chr,
                                     raw_fls_dir_1L_chr,
                                     processed_fls_dir_1L_chr,
                                     crs_nbr_dbl = NA_real_,
                                     overwrite_1L_lgl = F){
  ready4use::assert_single_row_tb(x)
  y_VicinityLookup <- VicinityLookup()
  y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_raw_r3",x)
  import_type_ls <- procure(x) ## NOT SURE IF THIS IS CORRECT / HAS BEEN DEFINED
  if(names(import_type_ls) == "script_chr"){
    make_class_fn_chr <- eval(parse(text = import_type_ls))
    script_args_ls <- list(lup_tbs_r4 = y_VicinityLookup,
                           merge_itms_chr = merge_itms_chr,
                           processed_fls_dir_1L_chr = processed_fls_dir_1L_chr,
                           raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                           pkg_1L_chr = package_1L_chr,
                           overwrite_1L_lgl = overwrite_1L_lgl,
                           crs_nbr_dbl = crs_nbr_dbl)
    z_VicinityArguments <- rlang::exec(make_class_fn_chr, !!!script_args_ls)
    return_xx <- manufacture(z_VicinityArguments)
  }else{
    return_xx <- VicinityLocalRaw(lup_tbs_r4 = y_VicinityLookup,
                                  merge_itms_chr = merge_itms_chr,
                                  raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                  pkg_1L_chr = package_1L_chr,
                                  overwrite_1L_lgl = overwrite_1L_lgl) %>% ## CLOSE CONDITIONAL, MOVE WHOLE CHUNK INTO REFORMED GET_IMPORT_TYPE_LS
      author(processed_fls_dir_1L_chr_chr = processed_fls_dir_1L_chr, # write_fls_from_imp_and_upd_r4
             crs_nbr_dbl = crs_nbr_dbl)
  }
  return(return_xx)
}
