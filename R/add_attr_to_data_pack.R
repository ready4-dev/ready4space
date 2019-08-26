#' @title add_attribute_to_data_pack_from_tb
#' @description FUNCTION_DESCRIPTION
#' @param attr_tb PARAM_DESCRIPTION
#' @param object_name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_attribute_to_data_pack_from_tb
#' @export
add_attribute_to_data_pack_from_tb <- function(attr_tb,
                                               object_name){
  eval(parse(text = paste0(object_name,
                           "<<-attr_tb")))
  eval(parse(text = paste0("usethis::use_data(",object_name,
                           ", overwrite = TRUE)")))
}
#### Duplicates of ymh.ced functions
#' @title add_attribute_to_data_pack
#' @description FUNCTION_DESCRIPTION
#' @param combined_ste_ppr_ls PARAM_DESCRIPTION
#' @param object_name_stub PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname add_attribute_to_data_pack
#' @export
#' @importFrom purrr walk
#' @importFrom stringr str_sub
add_attribute_to_data_pack <- function(combined_ste_ppr_ls,
                                       object_name_stub){
  load_attribute_to_global(combined_ste_ppr_ls = combined_ste_ppr_ls,
                           object_name_stub = object_name_stub)
  purrr::walk(names(combined_ste_ppr_ls),
              ~ eval(parse(text = paste0("usethis::use_data(",object_name_stub,.x %>% stringr::str_sub(start = 2),
                                         ", overwrite = TRUE)"))))
}

#' @title load_attribute_to_global
#' @description FUNCTION_DESCRIPTION
#' @param combined_ste_ppr_ls PARAM_DESCRIPTION
#' @param object_name_stub PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname load_attribute_to_global
#' @export
#' @importFrom purrr walk
#' @importFrom stringr str_sub
load_attribute_to_global <- function(combined_ste_ppr_ls,
                                     object_name_stub){
  purrr::walk(names(combined_ste_ppr_ls),
              ~ eval(parse(text = paste0(object_name_stub,.x %>% stringr::str_sub(start = 2),
                                         "<<-combined_ste_ppr_ls$",.x))))
}
#' @title add_ppr_ls_to_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_ppr_ls_to_data_pack_lup
#' @export
add_ppr_ls_to_data_pack_lup <- function(x,y){ ## update with names and check calling function(s).
  add_ppr_to_data_pack_lup(data_pack_lup = x,
                           combined_ste_ppr_ls = y[[1]],
                           object_name_stub = y[[2]],
                           area_type = y[[3]],
                           area_bound_yr = y[[4]],
                           region = y[[5]])
}
#' @title add_ppr_to_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param data_pack_lup PARAM_DESCRIPTION
#' @param combined_ste_ppr_ls PARAM_DESCRIPTION
#' @param object_name_stub PARAM_DESCRIPTION
#' @param area_type PARAM_DESCRIPTION
#' @param area_bound_yr PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[dplyr]{bind}}
#' @rdname add_ppr_to_data_pack_lup
#' @export
#' @importFrom tibble tibble
#' @importFrom stringr str_sub
#' @importFrom dplyr bind_rows
add_ppr_to_data_pack_lup <- function(data_pack_lup,
                                     combined_ste_ppr_ls,
                                     object_name_stub,
                                     area_type,
                                     area_bound_yr,
                                     region){
  tibble::tibble(name = paste0(object_name_stub,names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2)),
                 country = "Australia", # Change to context based method (lookup table)
                 area_type = area_type,
                 area_bound_yr = area_bound_yr,
                 region = region,
                 data_type = "Attribute",
                 main_feature = "Population projections",
                 year = names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2),
                 source_reference = paste0(object_name_stub,names(combined_ste_ppr_ls) %>% stringr::str_sub(start = 2))) %>%
    dplyr::bind_rows(data_pack_lup,
                     .)
}
#' @title add_attr_tb_to_data_pack_lup_from_arg_list
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname add_attr_tb_to_data_pack_lup_from_arg_list
#' @export
add_attr_tb_to_data_pack_lup_from_arg_list <- function(x,y){ ## Replace with names based referencing.
  add_attr_tb_to_data_pack_lup(data_pack_lup = x,
                               attr_tb = y[[1]],
                               object_name = y[[2]],
                               area_type = y[[3]],
                               area_bound_yr = y[[4]],
                               region = y[[5]],
                               year = y[[6]],
                               year_start = y[[7]],
                               year_end = y[[8]],
                               main_feature = y[[9]])
}
#' @title add_attr_tb_to_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param data_pack_lup PARAM_DESCRIPTION
#' @param attr_tb PARAM_DESCRIPTION
#' @param object_name PARAM_DESCRIPTION
#' @param area_type PARAM_DESCRIPTION
#' @param area_bound_yr PARAM_DESCRIPTION
#' @param region PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param year_start PARAM_DESCRIPTION
#' @param year_end PARAM_DESCRIPTION
#' @param main_feature PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[dplyr]{bind}}
#' @rdname add_attr_tb_to_data_pack_lup
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
add_attr_tb_to_data_pack_lup <- function(data_pack_lup,
                                         attr_tb,
                                         object_name,
                                         area_type,
                                         area_bound_yr,
                                         region,
                                         year,
                                         year_start,
                                         year_end,
                                         main_feature){ # replace with names based referencing
  tibble::tibble(name = object_name,
                 country = "Australia", # Pull this from context data.
                 area_type = area_type,
                 area_bound_yr = area_bound_yr,
                 region = region,
                 data_type = "Attribute",
                 main_feature = main_feature,
                 year = year,
                 year_start = year_start,
                 year_end = year_end,
                 source_reference = object_name) %>%
    dplyr::bind_rows(data_pack_lup,
                     .)
}
