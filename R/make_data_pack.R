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
#' @param lup_dir PARAM_DESCRIPTION, Default: 'data'
#' @param processed_dir PARAM_DESCRIPTION, Default: 'data'
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
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[ready4utils]{add_all_tbs_in_r4}}
#'  \code{\link[dplyr]{slice}}
#' @rdname make_data_packs.ready4_sp_import_lup
#' @export
#' @importFrom ready4s4 ready4_lookup
#' @importFrom purrr reduce
#' @importFrom ready4utils add_all_tbs_in_r4
#' @importFrom dplyr slice
make_data_packs.ready4_sp_import_lup <- function(x,
                            init_lookup_r4 = NULL,
                            pckg_name,
                            raw_data_dir,
                            lup_dir  = "data",
                            processed_dir = "data",
                            lup_r4_name){
  if(is.null(init_lookup_r4))
    init_lookup_r4 <- ready4s4::ready4_lookup()
  x <- x %>% add_names()
  lookup_tbs_r4 <- purrr::reduce(1:nrow(x),
                                 .init = init_lookup_r4,
                                 # merge_with_vec,
                                 ~ ready4utils::add_all_tbs_in_r4(r4_1 = .x,
                                                                  r4_2 = x %>% dplyr::slice(.y) %>%
                                                                    make_data_pack_sngl(merge_with = get_merge_sf_str(lookup_r4 = .x,
                                                                                                                      sp_import_r3_slice = x %>% dplyr::slice(.y),
                                                                                                                      processed_dir = processed_dir),#merge_with_vec[.y],
                                                                                        pckg_name = pckg_name,
                                                                                        raw_data_dir = raw_data_dir,
                                                                                        lup_dir = lup_dir,
                                                                                        processed_dir = processed_dir),
                                                                  r4_name = "ready4_lookup"))
  saveRDS(lookup_tbs_r4,file = paste0(lup_dir,"/",lup_r4_name,".rds"))
  return(lookup_tbs_r4)
}

#' @title make_data_pack_sngl
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param merge_with PARAM_DESCRIPTION
#' @param pckg_name PARAM_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @param lup_dir PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
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
                                lup_dir,
                                processed_dir){
  lookup_tbs_r4 <- ready4s4::ready4_lookup()
  lookup_tbs_r4 <- ready4s4::`sp_import_lup<-`(lookup_tbs_r4,x)
  if(x %>% dplyr::pull(data_type) == "Shape"){
    boundary_ls <- import_boundary_ls(lookup_tbs_r4,
                                      raw_data_dir)
    lookup_tbs_r4 <- export_starter_sf(lookup_tbs_r4,
                                       boundary_ls = boundary_ls,
                                       processed_dir = processed_dir,
                                       merge_with = merge_with) %>%
      export_uid_lup()
    lookup_tbs_r4 <-lookup_tbs_r4 %>%
      export_data_pack_lup(template_ls = boundary_ls,
                           tb_data_type = "Shape",
                           pckg_name = pckg_name,
                           lup_dir = lup_dir)
  }
  if(x %>% dplyr::pull(data_type) == "Attribute"){
    attribute_ls <- import_attribute_ls(lookup_tbs_r4,
                                        raw_data_dir)
    purrr::walk2(attribute_ls,
                 names(attribute_ls),
                 ~ export_attr_tb(attr_tb = .x,
                                  obj_name = .y,
                                  processed_dir = processed_dir))
    lookup_tbs_r4 <-lookup_tbs_r4 %>%
      #export_uid_lup() %>% ## NECESSARY?
      export_data_pack_lup(template_ls = attribute_ls,
                           tb_data_type = "Attribute",
                           pckg_name = pckg_name,
                           lup_dir = lup_dir)
  }
  return(lookup_tbs_r4)
}
#' @title export_attr_tb
#' @description FUNCTION_DESCRIPTION
#' @param attr_tb PARAM_DESCRIPTION
#' @param obj_name PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
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
                           processed_dir){
  saveRDS(attr_tb, file = paste0(processed_dir,"/",obj_name,".rds"))
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
  x %>%
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
                                                  ifelse(..4 == "Shape",
                                                         ifelse(..5 == "Boundary","_bnd_","_crd_"),
                                                         paste0("_",tolower(..5),"_")),
                                                  ..6
                                         )))
}
#' @title import_boundary_ls
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{setup_io_directories}},\code{\link[ready4utils]{c("data_import_selected_downloads", "data_import_selected_downloads")}},\code{\link[ready4utils]{c("data_import_items", "data_import_items")}}
#'  \code{\link[ready4s4]{sp_import_lup}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[stats]{setNames}}
#' @rdname import_boundary_ls
#' @export
#' @importFrom ready4utils setup_io_directories data_import_selected_downloads data_import_items
#' @importFrom ready4s4 sp_import_lup
#' @importFrom dplyr filter pull
#' @importFrom stats setNames
import_boundary_ls <- function(lookup_tbs_r4,
                               raw_data_dir){
  ready4utils::setup_io_directories(raw_data_dir)
  dir.create(paste0(raw_data_dir,"/InputData/SpatialData"))
  boundaries_to_import_vec <- ready4s4::sp_import_lup(lookup_tbs_r4) %>%
    dplyr::filter(main_feature == "Boundary") %>% dplyr::pull(name)
  ready4utils::data_import_selected_downloads(required_data = boundaries_to_import_vec,
                                              destination_directory = paste0(raw_data_dir,"/InputData/SpatialData"),
                                              sp_data_import_tb = ready4s4::sp_import_lup(lookup_tbs_r4))
  ready4utils::data_import_items(included_items_names = boundaries_to_import_vec,
                                 item_data_type = "Shape",
                                 data_directory = paste0(raw_data_dir,"/InputData/SpatialData"),
                                 sp_data_import_tb = ready4s4::sp_import_lup(lookup_tbs_r4))  %>%
    stats::setNames(boundaries_to_import_vec)
}
#' @title import_attribute_ls
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param raw_data_dir PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{setup_io_directories}},\code{\link[ready4utils]{c("data_import_selected_downloads", "data_import_selected_downloads")}},\code{\link[ready4utils]{c("data_import_items", "data_import_items")}}
#'  \code{\link[ready4s4]{sp_import_lup}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[stats]{setNames}}
#' @rdname import_attribute_ls
#' @export
#' @importFrom ready4utils setup_io_directories data_import_selected_downloads data_import_items
#' @importFrom ready4s4 sp_import_lup
#' @importFrom dplyr filter pull
#' @importFrom stats setNames
import_attribute_ls <- function(lookup_tbs_r4,
                               raw_data_dir){ ## Merge with import_boundary_ls
  ready4utils::setup_io_directories(raw_data_dir)
  dir.create(paste0(raw_data_dir,"/InputData/AttributeData"))
  attributes_to_import_vec <- ready4s4::sp_import_lup(lookup_tbs_r4) %>%
    dplyr::filter(data_type == "Attribute") %>% dplyr::pull(name)
  ready4utils::data_import_selected_downloads(required_data = attributes_to_import_vec,
                                              destination_directory = paste0(raw_data_dir,"/InputData/AttributeData"),
                                              sp_data_import_tb = ready4s4::sp_import_lup(lookup_tbs_r4))
  ready4utils::data_import_items(included_items_names = attributes_to_import_vec,
                                 item_data_type = "Attribute",
                                 data_directory = paste0(raw_data_dir,"/InputData/AttributeData"),
                                 sp_data_import_tb = ready4s4::sp_import_lup(lookup_tbs_r4))  %>%
    stats::setNames(attributes_to_import_vec)
}
#' @title export_starter_sf
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param boundary_ls PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
#' @param merge_with PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{geos_binary_ops}},\code{\link[sf]{geos_measures}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[units]{set_units}}
#'  \code{\link[tibble]{add_row}}
#'  \code{\link[ready4s4]{sp_starter_sf_lup}},\code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_starter_sf_lup<-}}
#' @rdname export_starter_sf
#' @export
#' @importFrom sf st_intersection st_area
#' @importFrom dplyr mutate filter pull
#' @importFrom units set_units
#' @importFrom tibble add_row
#' @importFrom ready4s4 sp_starter_sf_lup sp_import_lup sp_starter_sf_lup<-
export_starter_sf <- function(lookup_tbs_r4,
                              boundary_ls,
                              processed_dir,
                              merge_with){
  ## 5. Create new SF object, which adds State and Territory Boundary data to Boundary data and add it to data pack for export.
  if(is.na(merge_with)){
    starter_sf <- boundary_ls[[1]]
  }else{
    starter_sf <- sf::st_intersection(eval(parse(text=merge_with)),
                                      boundary_ls[[1]])
    starter_sf <- starter_sf %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::filter(area > units::set_units(0,m^2))
  }
  starter_sf_name <- paste0(names(boundary_ls)[1], #%>%
                            #stringr::str_replace("_bound_","_bnd_"),
                            "_sf")
  saveRDS(starter_sf, file = paste0(processed_dir,"/",starter_sf_name,".rds"))
  #usethis::use_data(starter_sf, overwrite = TRUE)
  ## 6. Create a starter SF lookup table with details of new SF object.
  starter_sf_lup_r3 <- tibble::add_row(ready4s4::sp_starter_sf_lup(lookup_tbs_r4),
                                       country = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(country),
                                       area_type = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(area_type),
                                       starter_sf = starter_sf_name,
                                       sf_main_sub_div = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid),
                                       # ready4s4::sp_import_lup(lookup_tbs_r4) %>%
                                       # dplyr::pull(add_boundaries) %>%
                                       # unlist() %>% ifelse(test = is.null(.),yes = NA_character_)
  ) ## Assumes length one list
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
                                year = "All",
                                var_name = ready4s4::sp_import_lup(lookup_tbs_r4) %>% dplyr::pull(uid))
  ready4s4::`sp_uid_lup<-`(lookup_tbs_r4, uid_lup_r3)
}
#' @title export_data_pack_lup
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param tb_data_type PARAM_DESCRIPTION, Default: 'Shape'
#' @param template_ls PARAM_DESCRIPTION, Default: NULL
#' @param pckg_name PARAM_DESCRIPTION
#' @param lup_dir PARAM_DESCRIPTION
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
#'  \code{\link[ready4utils]{data_get}},\code{\link[ready4utils]{c("add_attr_tb_to_data_pack_lup_from_arg_list", "add_attr_tb_to_data_pack_lup_from_arg_list")}}
#'  \code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_data_pack_lup}},\code{\link[ready4s4]{sp_data_pack_lup<-}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname export_data_pack_lup
#' @export
#' @importFrom purrr map2 reduce map2_chr
#' @importFrom ready4utils data_get add_attr_tb_to_data_pack_lup_from_arg_list
#' @importFrom ready4s4 sp_import_lup sp_data_pack_lup sp_data_pack_lup<-
#' @importFrom dplyr mutate
#' @importFrom stringr str_sub
export_data_pack_lup <- function(lookup_tbs_r4,
                                 tb_data_type = "Shape",
                                 template_ls = NULL,
                                 pckg_name,
                                 lup_dir){
  ## 8. Create data pack lookup table.
  ###
  data_pk_lup_arguments_ls <- purrr::map2(template_ls,
                                          names(template_ls),
                                          ~ list(.x,
                                                 .y,
                                                 ready4utils::data_get(data_lookup_tb = lookup_tbs_r4 %>%
                                                                         ready4s4::sp_import_lup(),
                                                                       target_variable = "area_type",
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
                                                                       target_variable = "main_feature",
                                                                       lookup_variable = "name",
                                                                       lookup_reference = .y,
                                                                       evaluate = FALSE)))
  data_pack_lup_r3 <- purrr::reduce(data_pk_lup_arguments_ls,
                                    .init = lookup_tbs_r4 %>%
                                      ready4s4::sp_data_pack_lup(),
                                    ~ ready4utils::add_attr_tb_to_data_pack_lup_from_arg_list(.x,.y)) %>%
    dplyr::mutate(data_type = tb_data_type) #####
  pckg_name <- ifelse(pckg_name =="",pckg_name, paste0(pckg_name,"::"))
  data_pack_lup_r3 <- data_pack_lup_r3 %>%
    dplyr::mutate(source_reference = paste0(pckg_name,source_reference))  %>%
    dplyr::mutate(source_reference = purrr::map2_chr(main_feature,
                                                     source_reference,
                                                     ~ ifelse(.x == "Boundary",
                                                              paste0(.y, # stringr::str_replace(.y,"_bound_","_bnd_")
                                                                     "_sf"),
                                                              .y)))
  ### TRANSFORMATION [Do In Context / Model]
  lookup_tbs_r4 <- ready4s4::`sp_data_pack_lup<-`(lookup_tbs_r4, data_pack_lup_r3)
  ## 9. Add Lookup Tables object to data pack for export.
  # data_pack_name <- paste0(names(template_ls)[1] %>% stringr::str_sub(end = 12), ##### CHECK IF DISTICNCT FROM ATTRIB
  #                          names(template_ls)[1] %>% stringr::str_sub(start = -4),
  #                          "_lup_r4")
  # saveRDS(lookup_tbs_r4,file = paste0(lup_dir,"/",data_pack_name,".rds"))
  lookup_tbs_r4
}
#' @title order_tb_ready4_sp_import_lup
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}},\code{\link[purrr]{reduce}}
#' @rdname order_tb_ready4_sp_import_lup
#' @export
#' @importFrom dplyr select mutate pull
#' @importFrom purrr map map2 reduce
order_tb_ready4_sp_import_lup <- function(x){
  ordering_tb <- x %>%
    dplyr::select(uid,add_boundaries) %>%
    dplyr::mutate(preceeded_by = purrr::map(add_boundaries,
                                            ~ unlist(.x)[unlist(.x) %in% uid]
    )) %>%
    dplyr::mutate(sequence = purrr::map2(preceeded_by,
                                         uid,
                                         ~ c(.x,.y)))
  ordering_vec <- purrr::reduce(ordering_tb %>%
                                  dplyr::pull(sequence),
                                ~ append(.x,.y[!.y %in% .x]))
  x[match(ordering_vec, x$uid),]
}
#' @title FUNCTION_TITLE
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
#'  \code{\link[purrr]{pluck}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[ready4s4]{sp_import_lup}},\code{\link[ready4s4]{sp_data_pack_lup}}
#'  \code{\link[stringr]{str_detect}}
#' @rdname get_merge_sf_str
#' @export
#' @importFrom purrr pluck
#' @importFrom ready4utils data_get
#' @importFrom ready4s4 sp_import_lup sp_data_pack_lup
#' @importFrom stringr str_detect
get_merge_sf_str <- function(lookup_r4,
                             sp_import_r3_slice,
                             processed_dir = NULL){
  if(is.null(sp_import_r3_slice %>% pull(add_boundaries) %>% purrr::pluck(1))){
    NA_character_
  }else{
    # uid_str <- sp_import_r3_slice %>% pull(add_boundaries) %>% purrr::pluck(1)
    sf_ref <- ready4utils::data_get(data_lookup_tb = ready4s4::sp_import_lup(lookup_r4),
                                    lookup_reference = sp_import_r3_slice %>% pull(add_boundaries) %>% purrr::pluck(1),
                                    lookup_variable = "uid",
                                    target_variable = "name",
                                    evaluate = FALSE) %>%
      ready4utils::data_get(data_lookup_tb = ready4s4::sp_data_pack_lup(lookup_r4),
                            lookup_reference = .,
                            lookup_variable = "name",
                            target_variable = "source_reference",
                            evaluate = FALSE)
    if(stringr::str_detect(sf_ref,"::")){
      st_ref
    }else{
      paste0("readRDS(\"",processed_dir,"/",sf_ref,".rds\")")
    }
  }
}
