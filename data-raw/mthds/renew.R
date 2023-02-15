renew.vicinity_processed <- function(x, ## add_attr_tb_to_processed_lup & add_attrs_to_processed_lup
                                     name_chr = character(0),
                                     country_chr = character(0),
                                     area_type_chr = character(0),
                                     area_bndy_yr_chr = character(0),
                                     region_chr = character(0),
                                     data_type_chr = character(0),
                                     main_feature_chr = character(0),
                                     year_chr = character(0),
                                     year_start_chr = character(0),
                                     year_end_chr = ycharacter(0),
                                     source_reference_chr = character(0),
                                     args_ls = NULL){
  if(!is.null(args_ls)){
    x <- add_attrs_to_processed_lup(data_pack_lup = x,
                                    #attr_tb = args_ls[[1]], # remove (carefully)
                                    object_name_1L_chr = args_ls[[2]],
                                    area_type_chr = args_ls[[3]],
                                    area_bndy_yr_chr = args_ls[[4]],
                                    region_chr = args_ls[[5]],
                                    year_chr = args_ls[[6]],
                                    year_start_chr = args_ls[[7]],
                                    year_end_chr = args_ls[[8]],
                                    main_feature_chr = args_ls[[9]])
  }else{
    fn_env_ls <- as.list(rlang::current_env())[-1]
    x <- ready4::update_tb_r3(x,
                              fn = renew.vicinity_processed,
                              fn_env_ls = fn_env_ls)

  }
  return(x)
}
