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
#'  \code{\link[ready4s4]{ready4_lookup}}
#'  \code{\link[ready4s3]{order_tb}}
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[ready4utils]{add_all_tbs_in_r4}}
#'  \code{\link[dplyr]{slice}}
#' @rdname make_data_packs.ready4_sp_import_lup
#' @export
#' @importFrom ready4s4 ready4_lookup
#' @importFrom ready4s3 order_tb
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
    init_lookup_r4 <- ready4s4::ready4_lookup()
  x <- x %>% add_names() %>%
    ready4s3::order_tb()
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
#'  \code{\link[ready4s4]{ready4_lookup}},\code{\link[ready4s4]{sp_import_lup<-}}
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map2}}
#' @rdname make_data_pack_sngl
#' @export
#' @importFrom ready4s4 ready4_lookup sp_import_lup<-
#' @importFrom dplyr pull
#' @importFrom purrr walk2
make_data_pack_sngl <- function(x,
                                merge_with,
                                pckg_name,
                                raw_data_dir,
                                processed_dir,
                                crs_nbr_vec = NA_real_,
                                overwrite_lgl = F){
  ready4use::assert_single_row_tb(x)
  lookup_tbs_r4 <- ready4s4::ready4_lookup()
  lookup_tbs_r4 <- ready4s4::`sp_import_lup<-`(lookup_tbs_r4,x)
  import_type_ls <- ready4use::get_import_type_ls(x)
  data_type_chr <- x %>% dplyr::pull(data_type)
  local_raw_r4 <- ready4s4::ready4_local_raw(lup_tbs_r4 = lookup_tbs_r4,
                                             merge_sfs_chr_vec = merge_with,
                                             raw_data_dir_chr = raw_data_dir,
                                             pckg_chr = pckg_name,
                                             overwrite_lgl = overwrite_lgl)
  if(names(import_type_ls) == "script_chr"){#!x %>% dplyr::pull(make_script_src) %>% is.na()
    import_xx <- ready4use::make_import_xx(x,
                                           script_args_ls = list(lup_tbs_r4 = lookup_tbs_r4,
                                                                 merge_sfs_chr_vec = merge_with,
                                                                 proc_data_dir_chr = processed_dir,
                                                                 raw_data_dir_chr = raw_data_dir,
                                                                 pckg_chr = pckg_name,
                                                                 overwrite_lgl = overwrite_lgl))
    lookup_tbs_r4 <- rlang::exec(!!rlang::sym(import_type_ls),
                                 import_xx)
  }else{
    local_proc_r4 <- save_raw(local_raw_r4)
    local_proc_r4 <- ready4s4::`proc_data_dir_chr<-`(local_proc_r4, processed_dir)
    import_data(local_proc_r4,
                crs_nbr_vec = crs_nbr_vec)
    # import_this_ls <- import_data(x = ready4s4::sp_import_lup(lookup_tbs_r4),
    #                               included_items_names = import_chr_vec,
    #                               item_data_type = data_type_chr,
    #                               data_directory = raw_format_sp_dir,
    #                               r_data_dir_chr = r_data_dir_chr,
    #                               save_lgl = save_lgl) %>%
    #   stats::setNames(import_chr_vec)
    #
    # if(x %>% dplyr::pull(data_type) == "Geometry"){
    #   path_to_starter_sf_chr <- get_r_import_path_chr(r_data_dir_chr = processed_dir,
    #                                                   name_chr = names(import_this_ls)[1],
    #                                                   data_type_chr = "Geometry")#paste0(processed_dir,"/",starter_sf_name,".rds")
    # }else{
    #   path_to_starter_sf_chr <- NA_character_
    # }
    # process_import_xx(x,
    #                   import_this_ls = import_this_ls,
    #                   path_to_starter_sf_chr = path_to_starter_sf_chr,
    #                   merge_with = merge_with,
    #                   pckg_name = pckg_name,
    #                   raw_data_dir = raw_data_dir,
    #                   processed_dir = processed_dir,
    #                   crs_nbr_vec = crs_nbr_vec,
    #                   overwrite_lgl = F)
    ###UPDATE
    if(x %>% dplyr::pull(data_type) == "Geometry"){
      lookup_tbs_r4 <- export_starter_sf(lookup_tbs_r4,
                                         path_to_starter_sf_chr = path_to_starter_sf_chr) %>%
        export_uid_lup()
    }
    #}
    lookup_tbs_r4 <- lookup_tbs_r4 %>%
      export_data_pack_lup(template_ls = import_this_ls,
                           tb_data_type = x %>% dplyr::pull(data_type),
                           pckg_name = pckg_name)
  }
  return(lookup_tbs_r4)
}
#' @title process_import_xx
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param import_this_ls PARAM_DESCRIPTION
#' @param path_to_starter_sf_chr PARAM_DESCRIPTION
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
#' @rdname process_import_xx
#' @export
#' @importFrom dplyr pull
#' @importFrom purrr walk2
process_import_xx <- function(x,
                        import_this_ls,
                        path_to_starter_sf_chr,
                        merge_with,
                        pckg_name,
                        raw_data_dir,
                        processed_dir,
                        crs_nbr_vec = NA_real_,
                        overwrite_lgl = F){
  if(x %>% dplyr::pull(data_type) == "Geometry"){
    process_geom_import(x,
                  import_this_ls = import_this_ls,
                  path_to_starter_sf_chr = path_to_starter_sf_chr,
                  processed_dir = processed_dir,
                  merge_with_chr_vec = merge_with_chr_vec,
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
process_geom_import <- function(x,#lookup_tbs_r4 %>% ready4s4::sp_import_lup()
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
                                                          validate_lgl = is.null(pnt_ls)))
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
#' #' @title import_ls
#' #' @description FUNCTION_DESCRIPTION
#' #' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' #' @param raw_data_dir PARAM_DESCRIPTION
#' #' @param r_data_dir_chr PARAM_DESCRIPTION
#' #' @param data_type_chr PARAM_DESCRIPTION, Default: 'Geometry'
#' #' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[ready4s4]{sp_import_lup}}
#' #'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#' #'  \code{\link[stats]{setNames}}
#' #' @rdname import_ls
#' #' @export
#' #' @importFrom ready4s4 sp_import_lup
#' #' @importFrom dplyr filter pull
#' #' @importFrom stats setNames
#' import_ls <- function(lookup_tbs_r4,
#'                       raw_data_dir,
#'                       r_data_dir_chr,
#'                       data_type_chr = "Geometry",
#'                       overwrite_lgl = F){
#'   raw_format_sp_dir <- make_raw_format_dir(data_type_chr = data_type_chr,
#'                                            raw_data_dir = raw_data_dir)
#'
#'   import_chr_vec <- get_import_chr_vec()
#'
#'   save_lgl <- save_raw(x = ready4s4::sp_import_lup(lookup_tbs_r4),
#'                        required_data = import_chr_vec,
#'                        destination_directory = raw_format_sp_dir,
#'                        overwrite_lgl = overwrite_lgl)
#'   import_data(x = ready4s4::sp_import_lup(lookup_tbs_r4),
#'               included_items_names = import_chr_vec,
#'               item_data_type = data_type_chr,
#'               data_directory = raw_format_sp_dir,
#'               r_data_dir_chr = r_data_dir_chr,
#'               save_lgl = save_lgl) %>%
#'     stats::setNames(import_chr_vec)
#' }
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
#'  \code{\link[ready4s4]{sp_import_lup}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#' @rdname get_import_chr_vec
#' @export
#' @importFrom ready4s4 sp_import_lup
#' @importFrom dplyr filter pull
get_import_chr_vec <- function(lookup_tbs_r4,
                               data_type_chr){
  if(data_type_chr == "Geometry"){
    ready4s4::sp_import_lup(lookup_tbs_r4) %>%
      dplyr::filter(main_feature == "Boundary") %>% dplyr::pull(name)
  }else{
    ready4s4::sp_import_lup(lookup_tbs_r4) %>%
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
#' #' @title import_attribute_ls
#' #' @description FUNCTION_DESCRIPTION
#' #' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' #' @param raw_data_dir PARAM_DESCRIPTION
#' #' @param overwrite_lgl PARAM_DESCRIPTION, Default: F
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[ready4s4]{sp_import_lup}}
#' #'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#' #'  \code{\link[stats]{setNames}}
#' #' @rdname import_attribute_ls
#' #' @export
#' #' @importFrom ready4s4 sp_import_lup
#' #' @importFrom dplyr filter pull
#' #' @importFrom stats setNames
#' import_attribute_ls <- function(lookup_tbs_r4,
#'                                 raw_data_dir,
#'                                 overwrite_lgl = F){ ## Merge with import_boundary_ls
#'   raw_format_att_dir <- make_raw_format_dir_str(raw_data_dir,"Attributes")
#'   if(!dir.exists(raw_format_att_dir))
#'     dir.create(raw_format_att_dir)
#'   attributes_to_import_vec <- ready4s4::sp_import_lup(lookup_tbs_r4) %>%
#'     dplyr::filter(data_type == "Attribute") %>% dplyr::pull(name)
#'   save_raw(x = ready4s4::sp_import_lup(lookup_tbs_r4),
#'            required_data = attributes_to_import_vec,
#'            destination_directory = raw_format_att_dir,
#'            overwrite_lgl = overwrite_lgl)
#'   import_data(x = ready4s4::sp_import_lup(lookup_tbs_r4),
#'               included_items_names = attributes_to_import_vec,
#'               item_data_type = "Attribute",
#'               data_directory = raw_format_att_dir,
#'               overwrite_lgl = overwrite_lgl)  %>%
#'     stats::setNames(attributes_to_import_vec)
#' }
#' @title export_starter_sf
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param boundary_ls PARAM_DESCRIPTION, Default: NULL
#' @param pnt_ls PARAM_DESCRIPTION, Default: NULL
#' @param processed_dir PARAM_DESCRIPTION
#' @param merge_with PARAM_DESCRIPTION
#' @param crs_nbr_vec PARAM_DESCRIPTION
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
#'  \code{\link[sf]{st_geometry_type}},\code{\link[sf]{geos_measures}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[units]{set_units}}
#'  \code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_starter_sf_lup}},\code{\link[ready4s4]{sp_starter_sf_lup<-}}
#'  \code{\link[tibble]{add_row}}
#' @rdname export_starter_sf
#' @export
#' @importFrom purrr reduce
#' @importFrom sf st_geometry_type st_area
#' @importFrom dplyr mutate filter pull
#' @importFrom units set_units
#' @importFrom ready4s4 sp_import_lup sp_starter_sf_lup sp_starter_sf_lup<-
#' @importFrom tibble add_row
export_starter_sf <- function(lookup_tbs_r4,
                              path_to_starter_sf_chr){
  starter_sf_name <- get_name_from_path_chr(path_to_starter_sf_chr, with_ext = F)
  starter_sf_lup_r3 <- tibble::add_row(ready4s4::sp_starter_sf_lup(lookup_tbs_r4),
                                       country = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(country),
                                       area_type = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type),
                                       starter_sf = starter_sf_name,
                                       sf_main_sub_div = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid)) ## Assumes length one list
  ready4s4::`sp_starter_sf_lup<-`(lookup_tbs_r4, starter_sf_lup_r3)
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
#'  \code{\link[ready4s3]{ready4_sp_uid_lup}}
#'  \code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_uid_lup<-}}
#'  \code{\link[dplyr]{pull}}
#' @rdname export_uid_lup
#' @export
#' @importFrom tibble add_row
#' @importFrom ready4s3 ready4_sp_uid_lup
#' @importFrom ready4s4 sp_import_lup sp_uid_lup<-
#' @importFrom dplyr pull
export_uid_lup <- function(lookup_tbs_r4){
  uid_lup_r3 <- tibble::add_row(ready4s3::ready4_sp_uid_lup(),
                                spatial_unit = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type),
                                year =  ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_bound_yr), ## "All".
                                var_name = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid))
  ready4s4::`sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
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
#'  \code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_data_pack_lup}},\code{\link[ready4s4]{sp_data_pack_lup<-}}
#'  \code{\link[dplyr]{mutate}}
#' @rdname export_data_pack_lup
#' @export
#' @importFrom purrr map2 reduce map2_chr
#' @importFrom ready4utils data_get
#' @importFrom ready4s4 sp_import_lup sp_data_pack_lup sp_data_pack_lup<-
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
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "area_type",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "area_bound_yr",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "region",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "year",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "year_start",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "year_end",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE),
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "main_feature",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE))) # Set names here to allow names based referencing in destination function.
  data_pack_lup_r3 <- purrr::reduce(data_pk_lup_arguments_ls,
                                    .init = lookup_tbs_r4 %>%
                                      ready4s4::sp_data_pack_lup(),
                                    ~ add_attr_tb_to_data_pack_lup_from_arg_list(.x,.y)) %>%
    dplyr::mutate(data_type = tb_data_type)
  pckg_name <- ifelse(pckg_name =="",pckg_name, paste0(pckg_name,"::"))
  data_pack_lup_r3 <- data_pack_lup_r3 %>%
    dplyr::mutate(source_reference = paste0(pckg_name,source_reference))  %>%
    dplyr::mutate(source_reference = purrr::map2_chr(main_feature,
                                                     source_reference,
                                                     ~ ifelse(.x == "Boundary",
                                                              paste0(.y,
                                                                     "_sf"),
                                                              .y)))
  lookup_tbs_r4 <- ready4s4::`sp_data_pack_lup<-`(lookup_tbs_r4, data_pack_lup_r3)
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
#'  \code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_data_pack_lup}}
#'  \code{\link[stringr]{str_detect}}
#' @rdname get_merge_sf_str
#' @export
#' @importFrom dplyr pull
#' @importFrom purrr pluck map_chr
#' @importFrom ready4utils data_get
#' @importFrom ready4s4 sp_import_lup sp_data_pack_lup
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
                     ~ ready4utils::data_get(data_lookup_tb = ready4s4::sp_import_lup(lookup_r4),
                                             lookup_reference = .x,
                                             lookup_variable = "uid",
                                             target_variable = "name",
                                             evaluate = FALSE) %>%
                       ready4utils::data_get(data_lookup_tb = ready4s4::sp_data_pack_lup(lookup_r4),
                                             lookup_reference = .,
                                             lookup_variable = "name",
                                             target_variable = "source_reference",
                                             evaluate = FALSE) %>%
                       ifelse(stringr::str_detect(.,"::"),.,paste0("readRDS(\"",processed_dir,"/",.,".rds\")")))
    }
  }
}
#' #' @title add_data_pack_from_script
#' #' @description FUNCTION_DESCRIPTION
#' #' @param x PARAM_DESCRIPTION
#' #' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' #' @param merge_sfs_vec PARAM_DESCRIPTION
#' #' @param processed_dir PARAM_DESCRIPTION
#' #' @param raw_data_dir PARAM_DESCRIPTION
#' #' @param pckg_name PARAM_DESCRIPTION
#' #' @param overwrite_lgl PARAM_DESCRIPTION
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[dplyr]{pull}}
#' #' @rdname add_data_pack_from_script
#' #' @export
#' #' @importFrom dplyr pull
#' add_data_pack_from_script <- function(x,
#'                                       lookup_tbs_r4,
#'                                       merge_sfs_vec,
#'                                       processed_dir,
#'                                       raw_data_dir,
#'                                       pckg_name,
#'                                       overwrite_lgl){
#'   parse(text = paste0(x %>% dplyr::pull(make_script_src),
#'                       "(x = x,
#'                       lookup_tbs_r4 = lookup_tbs_r4,
#'                       merge_sfs_vec = merge_sfs_vec,
#'                       processed_dir = processed_dir,
#'                       raw_data_dir = raw_data_dir,
#'                       pckg_name = pckg_name,
#'                       overwrite_lgl = overwrite_lgl)")) %>% eval()
#' }
