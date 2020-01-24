#' @title make_data_packs
#' @description Generic function to make data packs
#' @param x PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_data_packs
#' @export
make_data_packs <- function(x,
                         ...){
  UseMethod("make_data_packs",x)
}

#' @title make_data_packs.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param init_lookup_r4 PARAM_DESCRIPTION, Default: NULL
#' @param pckg_name PARAM_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION, Default: 'data'
#' @param lup_r4_name PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION, Default: NA
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[ready4utils]{add_all_tbs_in_r4}}
#'  \code{\link[dplyr]{slice}}
#' @rdname make_data_packs.ready4_sp_import_lup
#' @export
#' @importFrom purrr reduce
#' @importFrom ready4utils add_all_tbs_in_r4
#' @importFrom dplyr slice
make_data_packs.ready4_sp_import_lup <- function(x,
                                                 init_lookup_r4 = NULL,
                                                 pckg_name,
                                                 raw_data_dir,
                                                 processed_dir = "data",
                                                 lup_r4_name,
                                                 crs_nbr_vec = NA_real_,
                                                 overwrite_lgl = F){
  if(is.null(init_lookup_r4))
    init_lookup_r4 <- ready4_lookup()
  x <- x %>% add_names() %>%
    order_tb()
  lookup_tbs_r4 <- purrr::reduce(1:nrow(x),
                                 .init = init_lookup_r4,
                                 ~ ready4utils::add_all_tbs_in_r4(r4_1 = .x,
                                                                  r4_2 = x %>% dplyr::slice(.y) %>%
                                                                    make_data_pack_sngl(merge_with = get_merge_sf_str(lookup_r4 = .x,
                                                                                                                      sp_import_r3_slice = x %>% dplyr::slice(.y),
                                                                                                                      processed_dir = processed_dir),#merge_with_vec[.y],
                                                                                        pckg_name = pckg_name,
                                                                                        raw_data_dir = raw_data_dir,
                                                                                        processed_dir = processed_dir,
                                                                                        crs_nbr_vec = crs_nbr_vec,
                                                                                        overwrite_lgl = overwrite_lgl),
                                                                  r4_name = "ready4_lookup"))
  return(lookup_tbs_r4)
}

#' @title make_data_pack_sngl
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param merge_with PARAM_DESCRIPTION
#' @param pckg_name PARAM_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION, Default: NA
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map2}}
#' @rdname make_data_pack_sngl
#' @export
#' @importFrom dplyr pull
#' @importFrom purrr walk2
make_data_pack_sngl <- function(x, ## MAKE METHOD
                                merge_with,
                                pckg_name,
                                raw_data_dir,
                                processed_dir,
                                crs_nbr_vec = NA_real_,
                                overwrite_lgl = F){
  ready4use::assert_single_row_tb(x)
  lookup_tbs_r4 <- ready4_lookup()
  lookup_tbs_r4 <- `sp_import_lup<-`(lookup_tbs_r4,x)
  import_type_ls <- ready4use::get_import_type_ls(x)
  if(names(import_type_ls) == "script_chr"){#!x %>% dplyr::pull(make_script_src) %>% is.na()
    make_class_fn_chr <- eval(parse(text = import_type_ls))
    script_args_ls <- list(lup_tbs_r4 = lookup_tbs_r4,
                           merge_with_chr_vec = merge_with,
                           proc_data_dir_chr = processed_dir,
                           raw_data_dir_chr = raw_data_dir,
                           pckg_chr = pckg_name,
                           overwrite_lgl = overwrite_lgl,
                           crs_nbr_vec = crs_nbr_vec)
    script_data_r4 <- rlang::exec(make_class_fn_chr, !!!script_args_ls)
    import_data(script_data_r4)
    # script_data_r4 <- ready4use::make_import_xx(x,
    #                                             script_args_ls = list(lup_tbs_r4 = lookup_tbs_r4,
    #                                                                   merge_sfs_chr_vec = merge_with,
    #                                                                   proc_data_dir_chr = processed_dir,
    #                                                                   raw_data_dir_chr = raw_data_dir,
    #                                                                   pckg_chr = pckg_name,
    #                                                                   overwrite_lgl = overwrite_lgl,
    #                                                                   crs_nbr_vec = crs_nbr_vec))
    # rlang::exec(!!rlang::sym(import_type_ls),
    #             script_data_r4)
  }else{
    ready4_sp_local_raw(lup_tbs_r4 = lookup_tbs_r4,
                               merge_with_chr_vec = merge_with,
                               raw_data_dir_chr = raw_data_dir,
                               pckg_chr = pckg_name,
                               overwrite_lgl = overwrite_lgl) %>% ## CLOSE CONDITIONAL, MOVE WHOLE CHUNK INTO REFORMED GET_IMPORT_TYPE_LS
    save_import_and_update(processed_dir_chr = processed_dir,
                           crs_nbr_vec = crs_nbr_vec)
  }
}

#' @title save_import_and_update
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param processed_dir_chr PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname save_import_and_update
#' @export
save_import_and_update <- function(x,
                                   processed_dir_chr,
                                   crs_nbr_vec){
  save_raw(x,
           return_r4_lgl = T) %>%
    ready4use::`proc_data_dir_chr<-`(processed_dir_chr) %>%
    import_data(crs_nbr_vec = crs_nbr_vec) %>%
    update_this()
}

#' @title process_import_xx
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param import_this_ls PARAM_DESCRIPTION
#' @param path_to_starter_sf_chr PARAM_DESCRIPTION
#' @param merge_with PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION, Default: NA
#' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map2}}
#' @rdname process_import_xx
#' @export
#' @importFrom dplyr pull
#' @importFrom purrr walk2
process_import_xx <- function(x,
                        import_this_ls,
                        path_to_starter_sf_chr,
                        merge_with,
                        processed_dir,
                        crs_nbr_vec = NA_real_,
                        overwrite_lgl = F){
  if(x %>% dplyr::pull(data_type) == "Geometry"){
    process_geom_import(x,
                  import_this_ls = import_this_ls,
                  path_to_starter_sf_chr = path_to_starter_sf_chr,
                  #processed_dir = processed_dir,
                  merge_with_chr_vec = merge_with,
                  crs_nbr_vec= crs_nbr_vec,
                  overwrite_lgl = overwrite_lgl)
  }
  if(x %>% dplyr::pull(data_type) == "Attribute"){
    purrr::walk2(import_this_ls,
                 names(import_this_ls),
                 ~ export_attr_tb(attr_tb = .x,
                                  obj_name = .y,
                                  processed_dir = processed_dir,
                                  overwrite_lgl = overwrite_lgl))

  }
}

#' @title process_geom_import
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param import_this_ls PARAM_DESCRIPTION
#' @param path_to_starter_sf_chr PARAM_DESCRIPTION
#' @param merge_with_chr_vec PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
#' @param overwrite_lgl PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4use]{assert_single_row_tb}}
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[sf]{st_geometry_type}},\code{\link[sf]{geos_measures}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[units]{set_units}}
#' @rdname process_geom_import
#' @export
#' @importFrom ready4use assert_single_row_tb
#' @importFrom purrr reduce
#' @importFrom sf st_geometry_type st_area
#' @importFrom dplyr mutate filter pull
#' @importFrom units set_units
process_geom_import <- function(x,#lookup_tbs_r4 %>% sp_import_lup()
                          import_this_ls,
                          path_to_starter_sf_chr,
                          #processed_dir,
                          merge_with_chr_vec,
                          crs_nbr_vec,
                          overwrite_lgl){
  ready4use::assert_single_row_tb(x)
  # if(!is.null(import_this_ls))
  #   geom_ls <- import_this_ls
  # if(!is.null(pnt_ls))
  #   geom_ls <- pnt_ls
  if(overwrite_lgl | !file.exists(path_to_starter_sf_chr)){
    if(is.na(merge_with_chr_vec) %>% all()){
      starter_sf <- import_this_ls[[1]]
    }else{
      starter_sf <- purrr::reduce(merge_with_chr_vec,
                                  .init = import_this_ls[[1]],
                                  ~ intersect_lon_lat_sfs(.x,
                                                          eval(parse(text=.y)),
                                                          crs_nbr_vec = crs_nbr_vec,
                                                          validate_lgl = is.null(pnt_ls))) ## DUD REF TO PNT_LS
      if((sf::st_geometry_type(starter_sf) %>% as.character()!="POINT") %>% any()){
        starter_sf <- starter_sf %>%
          dplyr::mutate(area = sf::st_area(.)) %>%
          dplyr::filter(area > units::set_units(0,m^2)) ## Note: Will discard points
      }
    }
    if(x %>% dplyr::pull(main_feature) == "Boundary")
      starter_sf <- starter_sf %>%
        simplify_sf(crs = crs_nbr_vec[1])
    saveRDS(starter_sf, file = path_to_starter_sf_chr)
  }
}

#' @title export_attr_tb
#' @description FUNCTION_DESCRIPTION
#' @param attr_tb PARAM_DESCRIPTION
#' @param obj_name PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
#' @param overwrite_lgl PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname export_attr_tb
#' @export
export_attr_tb <- function(attr_tb,
                           obj_name,
                           processed_dir,
                           overwrite_lgl){
  path_to_attr_tb_chr <- get_r_import_path_chr(r_data_dir_chr = processed_dir,
                                               name_chr = obj_name,
                                               data_type_chr = "Attribute")#paste0(processed_dir,"/",obj_name,".rds")
  if(overwrite_lgl | !file.exists(path_to_attr_tb_chr))
  saveRDS(attr_tb, file = path_to_attr_tb_chr)
}

#' @title get_r_import_path_chr
#' @description FUNCTION_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @param name_chr PARAM_DESCRIPTION
#' @param data_type_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_r_import_path_chr
#' @export
get_r_import_path_chr <- function(r_data_dir_chr,
                                  name_chr,
                                  data_type_chr){
  if(data_type_chr=="Geometry")
    name_chr <- paste0(name_chr,"_sf")

  paste0(r_data_dir_chr,"/",name_chr,".rds")

}

#' @title add_names
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname add_names
#' @export
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_chr
#' @importFrom ready4utils data_get
#' @importFrom stringr str_sub
#' @import ISOcodes
add_names <- function(x){
  data(ISO_3166_1, package = "ISOcodes", envir = environment())
  x <- x %>%
    dplyr::mutate(name = purrr::pmap_chr(list(country,
                                              area_type,
                                              region,
                                              data_type,
                                              main_feature,
                                              year),
                                         ~ paste0(ready4utils::data_get(data_lookup_tb = ISO_3166_1,
                                                                        lookup_reference = ..1,
                                                                        lookup_variable = "Name",
                                                                        target_variable = "Alpha_3",
                                                                        evaluate = FALSE) %>% tolower(),
                                                  "_",
                                                  tolower(..2),
                                                  "_",
                                                  tolower(..3 %>% stringr::str_sub(end=3)),
                                                  ifelse(..4 == "Geometry",
                                                         ifelse(..5 == "Boundary","_bnd_","_crd_"),
                                                         paste0("_",tolower(..5),"_")),
                                                  ..6
                                         )))
  x %>% dplyr::mutate(name = make.unique(name)) %>% dplyr::mutate(name = map_chr(name, ~ ifelse(stringr::str_sub(.x,start = -2, end = -2) == ".",
                                                                                                paste0(stringr::str_sub(.x, end = 11),
                                                                                                       stringr::str_sub(.x,start = -1),
                                                                                                       stringr::str_sub(.x, start = 12, end = -3)),
                                                                                                .x)))
}

#'
#' @title make_raw_format_dir
#' @description FUNCTION_DESCRIPTION
#' @param data_type_chr PARAM_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_raw_format_dir
#' @export
make_raw_format_dir <- function(data_type_chr,
                                raw_data_dir){
  directory_chr <- switch(data_type_chr, "Geometry" = "Geometries","Attribute" = "Attributes")
  # main_ft_chr <- switch(data_type_chr, "Geometry" = "Geometries","Attribute" = "Attributes")
  raw_format_sp_dir <- make_raw_format_dir_str(raw_data_dir,directory_chr)
  if(!dir.exists(raw_format_sp_dir))
    dir.create(raw_format_sp_dir)
  raw_format_sp_dir
}

#' @title get_import_chr_vec
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param data_type_chr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#' @rdname get_import_chr_vec
#' @export
#' @importFrom dplyr filter pull
get_import_chr_vec <- function(lookup_tbs_r4,
                               data_type_chr){
  if(data_type_chr == "Geometry"){
    sp_import_lup(lookup_tbs_r4) %>%
      dplyr::filter(main_feature == "Boundary") %>% dplyr::pull(name)
  }else{
    sp_import_lup(lookup_tbs_r4) %>%
      dplyr::filter(data_type == "Attribute") %>% dplyr::pull(name)
  }
}

#' @title make_raw_format_dir_str
#' @description FUNCTION_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @param category PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname make_raw_format_dir_str
#' @export
make_raw_format_dir_str <- function(raw_data_dir,
                                    category){
  paste0(raw_data_dir,"/",category)
}

#' @title export_starter_sf
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param path_to_starter_sf_chr A character string.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[sf]{st_geometry_type}},\code{\link[sf]{geos_measures}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[units]{set_units}}
#'  \code{\link[tibble]{add_row}}
#' @rdname export_starter_sf
#' @export
#' @importFrom purrr reduce
#' @importFrom sf st_geometry_type st_area
#' @importFrom dplyr mutate filter pull
#' @importFrom units set_units
#' @importFrom tibble add_row
export_starter_sf <- function(lookup_tbs_r4,
                              path_to_starter_sf_chr){
  starter_sf_name <- get_name_from_path_chr(path_to_starter_sf_chr, with_ext = F)
  starter_sf_lup_r3 <- tibble::add_row(sp_starter_sf_lup(lookup_tbs_r4),
                                       country = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(country),
                                       area_type = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type),
                                       starter_sf = starter_sf_name,
                                       sf_main_sub_div = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid)) ## Assumes length one list
  `sp_starter_sf_lup<-`(lookup_tbs_r4, starter_sf_lup_r3)
}

#' @title export_uid_lup
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[tibble]{add_row}}
#'  \code{\link[dplyr]{pull}}
#' @rdname export_uid_lup
#' @export
#' @importFrom tibble add_row
#' @importFrom dplyr pull
export_uid_lup <- function(lookup_tbs_r4){
  uid_lup_r3 <- tibble::add_row(ready4_sp_uid_lup(),
                                spatial_unit = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type),
                                year =  sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_bound_yr), ## "All".
                                var_name = sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid))
  `sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
}

#' @title export_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param tb_data_type PARAM_DESCRIPTION, Default: 'Geometry'
#' @param template_ls PARAM_DESCRIPTION, Default: NULL
#' @param pckg_name PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}},\code{\link[purrr]{reduce}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[dplyr]{mutate}}
#' @rdname export_data_pack_lup
#' @export
#' @importFrom purrr map2 reduce map2_chr
#' @importFrom ready4utils data_get
#' @importFrom dplyr mutate
export_data_pack_lup <- function(lookup_tbs_r4,
                                 tb_data_type = "Geometry",
                                 template_ls = NULL,
                                 pckg_name){
  data_pk_lup_arguments_ls <- purrr::map2(template_ls, # remove (carefully)
                                          names(template_ls),
                                          ~ list(.x, # remove (carefully)
                                                 .y,
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "area_type",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "area_bound_yr",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "region",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "year",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "year_start",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "year_end",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         sp_import_lup(),
                                                                       target_variable = "main_feature",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE))) # Set names here to allow names based referencing in destination function.
  data_pack_lup_r3 <- purrr::reduce(data_pk_lup_arguments_ls,
                                    .init = lookup_tbs_r4 %>%
                                      sp_data_pack_lup(),
                                    ~ add_attr_tb_to_data_pack_lup_from_arg_list(.x,.y)) %>%
    dplyr::mutate(data_type = tb_data_type)
  pckg_name <- ifelse((pckg_name ==""|is.na(pckg_name)),"", paste0(pckg_name,"::"))
  data_pack_lup_r3 <- data_pack_lup_r3 %>%
    dplyr::mutate(source_reference = paste0(pckg_name,source_reference))  %>%
    dplyr::mutate(source_reference = purrr::map2_chr(main_feature,
                                                     source_reference,
                                                     ~ ifelse(.x == "Boundary",
                                                              paste0(.y,
                                                                     "_sf"),
                                                              .y)))
  lookup_tbs_r4 <- `sp_data_pack_lup<-`(lookup_tbs_r4, data_pack_lup_r3)
  lookup_tbs_r4
}

#' @title get_merge_sf_str
#' @description FUNCTION_DESCRIPTION
#' @param lookup_r4 PARAM_DESCRIPTION
#' @param sp_import_r3_slice PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{pluck}},\code{\link[purrr]{map}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[stringr]{str_detect}}
#' @rdname get_merge_sf_str
#' @export
#' @importFrom dplyr pull
#' @importFrom purrr pluck map_chr
#' @importFrom ready4utils data_get
#' @importFrom stringr str_detect
get_merge_sf_str <- function(lookup_r4,
                             sp_import_r3_slice,
                             processed_dir = NULL){
  if(is.null(sp_import_r3_slice %>% dplyr::pull(add_boundaries) %>% purrr::pluck(1))){
    NA_character_
  }else{
    if(is.na(sp_import_r3_slice %>% dplyr::pull(add_boundaries) %>% purrr::pluck(1)) %>% any()){
      NA_character_
    }else{
      purrr::map_chr(sp_import_r3_slice %>% pull(add_boundaries) %>% purrr::pluck(1),
                     ~ ready4utils::data_get(data_lookup_tb = sp_import_lup(lookup_r4),
                                             lookup_reference = .x,
                                             lookup_variable = "uid",
                                             target_variable = "name",
                                             evaluate = FALSE) %>%
                       ready4utils::data_get(data_lookup_tb = sp_data_pack_lup(lookup_r4),
                                             lookup_reference = .,
                                             lookup_variable = "name",
                                             target_variable = "source_reference",
                                             evaluate = FALSE) %>%
                       ifelse(stringr::str_detect(.,"::"),.,paste0("readRDS(\"",processed_dir,"/",.,".rds\")")))
    }
  }
}

