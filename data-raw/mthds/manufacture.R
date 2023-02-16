manufacture.vicinity_raw <- function(x, ## write_fls_and_mk_sngl_row_data_lup
                                     crs_nbr_dbl = NA_real_,
                                     merge_itms_chr = character(0),
                                     overwrite_1L_lgl = F,
                                     package_1L_chr  = character(0),
                                     processed_fls_dir_1L_chr  = character(0),
                                     raw_fls_dir_1L_chr  = character(0),
                                     what_1L_chr = "output"
                                     ){
  if(what_1L_chr == "output"){
    ready4use::assert_single_row_tb(x)
    y_VicinityLookup <- VicinityLookup()
    y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_raw_r3",x)
    import_type_ls <- procure.vicinity_raw(x,
                                           inc_script_1L_lgl = T,
                                           forced_choice_chr = NA_character_,
                                           what_1L_chr = "source") ####TF2A
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
      object_xx <- manufacture(z_VicinityArguments)
    }else{
      object_xx <- VicinityLocalRaw(lup_tbs_r4 = y_VicinityLookup,
                                    merge_itms_chr = merge_itms_chr,
                                    raw_fls_dir_1L_chr = raw_fls_dir_1L_chr,
                                    pkg_1L_chr = package_1L_chr,
                                    overwrite_1L_lgl = overwrite_1L_lgl) %>% ## CLOSE CONDITIONAL, MOVE WHOLE CHUNK INTO REFORMED GET_IMPORT_TYPE_LS
        author(processed_fls_dir_1L_chr_chr = processed_fls_dir_1L_chr, # write_fls_from_imp_and_upd_r4
               crs_nbr_dbl = crs_nbr_dbl)
    }
  }
  if(what_1L_chr == "source"){
    assert_single_row_tb(x)
    import_type_ls <- ready4::procure(x,
                                      inc_script_1L_lgl = !is.null(script_args_ls),
                                      forced_choice_chr = forced_choice_chr,
                                      what_1L_chr = "source")
    object_xx <- switch(names(import_type_ls),
                        "script_chr" = rlang::exec(VicinityArguments, x, !!!script_args_ls), ## THIS LOOKS WRONG - Extra slot needed? Function required? ###TF2A
                        "local_chr" = ready4use::get_valid_path_chr(import_type_ls[[1]]),
                        "repo_chr"  = ready4use::manufacture.ready4use_dataverses(x %>% dplyr::select(names(ready4use::ready4use_dataverses())) %>%
                                                                                    tibble::as_tibble() %>%
                                                                                    ready4use::ready4use_dataverses() ## From ready4use manufacture mthd
                        ),
                        "source_url_chr" = url(import_type_ls[[1]])
    )
  }
  if(what_1L_chr == "ingest"){ #make_import_object
    object_xx <- NULL  # could pass custom fn to this method
    stop("A Make Import Object Method needs to be defined for the child class of vicinity_raw.") # Remove?
  }

  return(object_xx)
}
