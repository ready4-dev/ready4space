
#' @title import_data.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param included_items_names PARAM_DESCRIPTION
#' @param item_data_type PARAM_DESCRIPTION
#' @param data_directory PARAM_DESCRIPTION
#' @param r_data_dir_chr PARAM_DESCRIPTION
#' @param save_lgl PARAM_DESCRIPTION, Default: T
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[sf]{st_read}}
#'  \code{\link[stats]{setNames}}
#' @rdname import_data.ready4_sp_import_lup
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_chr map
#' @importFrom ready4use import_data
#' @importFrom sf st_read
#' @importFrom stats setNames
import_data.ready4_sp_import_lup <- function(x, # NOTE, WHEN DOCUMENTING: IMPORTS GENERIC
                                             included_items_names,
                                             item_data_type,
                                             data_directory,
                                             r_data_dir_chr,
                                             save_lgl = T){
  downloaded_data_tb <- x %>%
    dplyr::filter(data_type == item_data_type) %>%
    dplyr::mutate(inc_file_main = ifelse(is.null(x$new_names_for_inc_files[[1]]),
                                         inc_file_main,
                                         ifelse(is.na(new_names_for_inc_files %>% unlist()),
                                                inc_file_main,
                                                purrr::map_chr(new_names_for_inc_files,
                                                               ~ .x[[1]]))))
  path_vec <- purrr::map_chr(included_items_names,
                             ~ data_import_get_one_path(downloaded_data_tb = downloaded_data_tb %>%
                                                          dplyr::select(c(name, country, area_type, region,
                                                                          #data_type,
                                                                          main_feature, year, inc_file_main)),
                                                        lookup_reference = .x,
                                                        data_directory = data_directory))
  r_import_path_chr <- get_r_import_path_chr(r_data_dir_chr = r_data_dir_chr,
                                             name_chr = x$name,
                                             data_type_chr = item_data_type)
  if(item_data_type=="Geometry"){
    item_list <- purrr::map(path_vec,
                            ~ {
                              if(!save_lgl & file.exists(r_import_path_chr)){
                                "SKIP_IMPORT"
                              }else{
                                sf::st_read(dsn=.x,
                                            layer = get_name_from_path_chr(.x,
                                                                                        with_ext = FALSE))
                              }
                                }
                            ) %>%
      stats::setNames(included_items_names)
  }else{
    item_list <- purrr::map(path_vec,
                            ~ {
                              if(!save_lgl & file.exists(r_import_path_chr)){
                                "SKIP_IMPORT"
                              }else{
                                data_import_non_shape_items(.x,
                                                            x = downloaded_data_tb)
                              }
                              }
                            ) %>%
      stats::setNames(included_items_names)
  }
  return(item_list)
}

#' @importMethodsFrom ready4use import_data
#' @export
methods::setMethod("import_data","ready4_sp_import_lup",import_data.ready4_sp_import_lup) # NOTE, BOTH EXTENDS GENERIC FROM OTHER PACKAGE AND DEFAULTS TO S3 METHOD

#' @importMethodsFrom ready4use import_data
#' @export
methods::setMethod("import_data",
                   "ready4_local_proc",
                   function(x,
                            crs_nbr_vec,
                            return_r4_lgl = T) {
                     sp_import_lup <- x@lup_tbs_r4@sp_import_lup
                     ready4use::assert_single_row_tb(sp_import_lup)
                     import_this_ls <- import_data(x = sp_import_lup,
                                                   included_items_names = x@import_chr_vec,
                                                   item_data_type = sp_import_lup$data_type,
                                                   data_directory = x@raw_data_dir_chr,
                                                   r_data_dir_chr = x@proc_data_dir_chr,
                                                   save_lgl = x@save_lgl) %>%
                       stats::setNames(x@import_chr_vec)
                     if(sp_import_lup$data_type == "Geometry"){
                       path_to_starter_sf_chr <- get_r_import_path_chr(r_data_dir_chr = x@proc_data_dir_chr,
                                                                       name_chr = names(import_this_ls)[1],
                                                                       data_type_chr = "Geometry")
                     }else{
                       path_to_starter_sf_chr <- NA_character_
                     }
                     process_import_xx(x = sp_import_lup,
                                       import_this_ls = import_this_ls,
                                       path_to_starter_sf_chr = path_to_starter_sf_chr,
                                       merge_with = x@merge_with_chr_vec,
                                       pckg_name = x@pckg_chr,
                                       raw_data_dir = raw_data_dir,
                                       crs_nbr_vec = crs_nbr_vec,
                                       overwrite_lgl = x@overwrite_lgl)
                     if(return_r4_lgl)
                       ready4s4::`path_to_starter_sf_chr<-`(x,path_to_starter_sf_chr) %>%
                       ready4s4::`import_this_ls<-`(import_this_ls)
                   }) # NOTE, EXTENDS GENERIC FROM OTHER PACKAGE


#' @title data_import_get_dir_paths
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION
#' @param directory_sub_divs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{accumulate}}
#'  \code{\link[ready4utils]{data_get}}
#' @rdname data_import_get_dir_paths
#' @export
#' @importFrom purrr map_chr accumulate
#' @importFrom ready4utils data_get
data_import_get_dir_paths <- function(x,
                                      destination_directory,
                                      data_lookup_ref,
                                      lookup_variable,
                                      directory_sub_divs){
  directory_names <- purrr::map_chr(directory_sub_divs,
                                    ~ ready4utils::data_get(data_lookup_tb = x,
                                                            lookup_reference = data_lookup_ref,
                                                            lookup_variable = lookup_variable,
                                                            target_variable = .x,
                                                            evaluate = FALSE))
  purrr::accumulate(directory_names,
                    ~ paste0(.x,
                             "/",
                             .y)) %>%
    paste0(destination_directory,
           "/",
           .)
}

#' @title data_import_make_directories
#' @description FUNCTION_DESCRIPTION
#' @param directory_paths PARAM_DESCRIPTION
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
#' @rdname data_import_make_directories
#' @export
#' @importFrom purrr walk
data_import_make_directories <- function(directory_paths){

  purrr::walk(directory_paths,
              ~ dir.create(.x))
}
#' @title data_import_save_files
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION
#' @param directory_path PARAM_DESCRIPTION
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
#'  \code{\link[purrr]{map}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[utils]{download.file}},\code{\link[utils]{unzip}}
#' @rdname data_import_save_files
#' @export
#' @importFrom purrr map_chr
#' @importFrom ready4utils data_get
#' @importFrom utils download.file unzip
data_import_save_files <- function(x,
                                   data_lookup_ref,
                                   lookup_variable,
                                   directory_path,
                                   overwrite_lgl = F){
  save_lgl <- F
  download_components_vec <- purrr::map_chr(c("file_name",
                                              "file_type",
                                              "download_url",
                                              "inc_file_main",
                                              "local_file_src"),
                                            ~ ready4utils::data_get(data_lookup_tb = x,
                                                                    lookup_reference = data_lookup_ref,
                                                                    lookup_variable = lookup_variable,
                                                                    target_variable = .x,
                                                                    evaluate = FALSE))
  dest_file <- paste0(directory_path,
                      "/",
                      download_components_vec[1],
                      download_components_vec[2])
  if(!is.na(download_components_vec[5])){
    if(overwrite_lgl | file.exists(dest_file))
      file.copy(from = download_components_vec[5],to = dest_file)
  }else{
    if(!is.na(paste0(directory_path,
                     "/",
                     download_components_vec[4]))){
      if(overwrite_lgl | !file.exists(paste0(directory_path, ## NEEDS UPDATING TO REFERENCE RENAMED PRE-EXISTING FILES
                             "/",
                             download_components_vec[4]))){
        utils::download.file(download_components_vec[3],
                             destfile = dest_file,
                             mode = 'wb')
        if(download_components_vec[2] == ".zip"){
          utils::unzip(dest_file,
                       exdir = directory_path)
        }
        data_import_rename_files(x = x,
                                 data_lookup_ref = data_lookup_ref,
                                 lookup_variable = lookup_variable,
                                 directory_path = directory_path,
                                 overwrite_lgl = overwrite_lgl)
        save_lgl <- T
      }
    }
  }
  save_lgl
}
#' @title data_import_rename_files
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION
#' @param directory_path PARAM_DESCRIPTION
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
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[purrr]{map2}}
#' @rdname data_import_rename_files
#' @export
#' @importFrom ready4utils data_get
#' @importFrom purrr walk2
data_import_rename_files <- function(x,
                                     data_lookup_ref,
                                     lookup_variable,
                                     directory_path,
                                     overwrite_lgl = F){
  old_names_list <- ready4utils::data_get(data_lookup_tb = x,
                                          lookup_reference = data_lookup_ref,
                                          lookup_variable = lookup_variable,
                                          target_variable = "inc_files_to_rename",
                                          evaluate = FALSE)
  new_names_list <- ready4utils::data_get(data_lookup_tb = x,
                                          lookup_reference = data_lookup_ref,
                                          lookup_variable = lookup_variable,
                                          target_variable = "new_names_for_inc_files",
                                          evaluate = FALSE)
  if(!is.na(old_names_list)){
    purrr::walk2(old_names_list,
                 new_names_list,
                 ~ if(overwrite_lgl | !file.exists(paste0(directory_path,
                                                          "/",
                                                          .y)))
                   file.rename(paste0(directory_path,
                                      "/",
                                      .x),
                               paste0(directory_path,
                                      "/",
                                      .y)))

  }
}

#' @title data_import_show_menu_detail
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
#'  \code{\link[dplyr]{select}}
#' @rdname data_import_show_menu_detail
#' @export
#' @importFrom dplyr select
data_import_show_menu_detail <- function(x){
  x %>%
    dplyr::select(c(1:8,12))
}

#' @title data_import_show_menu_of_type_detail
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param lookup_ref PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#' @rdname data_import_show_menu_of_type_detail
#' @export
#' @importFrom dplyr filter
data_import_show_menu_of_type_detail <- function(x,
                                                 lookup_ref){
  #data_import_show_menu_detail(x = x) %>%
  x %>%
    dplyr::filter(data_type==lookup_ref)
}

#' @title data_import_show_menu_names
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
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}}
#' @rdname data_import_show_menu_names
#' @export
#' @importFrom dplyr select pull
data_import_show_menu_names <- function(x){
  # data_import_show_menu_detail(x = x) %>%
  x %>%
    dplyr::select(name) %>%
    dplyr::pull()
}

#' @title data_import_show_menu_of_type_names
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param lookup_ref PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}}
#' @rdname data_import_show_menu_of_type_names
#' @export
#' @importFrom dplyr select pull
data_import_show_menu_of_type_names <- function(x,
                                                lookup_ref){
  data_import_show_menu_of_type_detail(x = x,
                                       lookup_ref = lookup_ref) %>%
    dplyr::select(name) %>%
    dplyr::pull()
}

#' @title data_import_get_one_path
#' @description FUNCTION_DESCRIPTION
#' @param downloaded_data_tb PARAM_DESCRIPTION
#' @param lookup_reference PARAM_DESCRIPTION
#' @param data_directory PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{select}}
#'  \code{\link[ready4utils]{data_get}}
#' @rdname data_import_get_one_path
#' @export
#' @importFrom purrr map_chr
#' @importFrom dplyr select
#' @importFrom ready4utils data_get
data_import_get_one_path <- function(downloaded_data_tb,
                                     lookup_reference,
                                     data_directory) {
  path_element_vector <- purrr::map_chr(downloaded_data_tb %>% dplyr::select(-name) %>% names(),
                                        ~ ready4utils::data_get(data_lookup_tb = downloaded_data_tb,
                                                                lookup_variable = "name",
                                                                lookup_reference = lookup_reference,
                                                                target_variable = .x,
                                                                evaluate = FALSE))
  paste0(data_directory,
         "/",
         paste(path_element_vector,collapse = "/"))
}

#' @title data_import_non_shape_items
#' @description FUNCTION_DESCRIPTION
#' @param path_str PARAM_DESCRIPTION
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
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[stringi]{stri_locate_all}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[purrr]{map}}
#' @rdname data_import_non_shape_items
#' @export
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
#' @importFrom ready4utils data_get
#' @importFrom purrr map_chr
data_import_non_shape_items <- function(path_str,
                                        x){
  file_name <-  get_name_from_path_chr(path_str)
  file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, "\\.")[,2] %>%
                                               as.vector())
  data_type <- ready4utils::data_get(data_lookup_tb = x,#data_import_show_menu_detail(x = x),
                                     lookup_reference = file_name,
                                     lookup_variable = "inc_file_main",
                                     target_variable = "data_type",
                                     evaluate = FALSE)
  var_name_vec <- c("area_type",
                    # "area_bound_yr",
                    "main_feature",
                    "year",
                    "region")
  var_val_vec <- purrr::map_chr(var_name_vec,
                                ~ ready4utils::data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
                                                                                                              x = x),
                                                        lookup_reference = file_name,
                                                        lookup_variable = "inc_file_main",
                                                        target_variable = .x,
                                                        evaluate = FALSE))
  # data_feature <- ready4utils::data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
  #                                                                                             x = x),
  #                                       lookup_reference = file_name,
  #                                       lookup_variable = "inc_file_main",
  #                                       target_variable = "main_feature",
  #                                       evaluate = FALSE) %>%
  #   stringr::str_sub(1,3)
  # area_type <- ready4utils::data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
  #                                                                                          x = x),
  #                                    lookup_reference = file_name,
  #                                    lookup_variable = "inc_file_main",
  #                                    target_variable = "area_type",
  #                                    evaluate = FALSE)
  make_import_obj_for_context(context = ready4utils::data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
                                                                                                                    x = x),
                                                              lookup_reference = file_name,
                                                              lookup_variable = "inc_file_main",
                                                              target_variable = "country",
                                                              evaluate = FALSE), # Replace with read from context attribute of lup object.
                              var_val_vec = var_val_vec,
                              path_str = path_str)
}

make_import_obj_for_context <- function(context,
                                        var_val_vec,
                                        path_str){
  ## Replace condition logic by class based methods
  if(context == "Australia")
    context <- "AusSpR4c"
  library(context, character.only=TRUE)
  x <- make_import_obj_for_australia(var_val_vec = var_val_vec,
                                     path_str = path_str)
  detach(paste0("package:",context), character.only = T)
  x
}



#' @param path_str PARAM_DESCRIPTION
#' @param with_ext PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[stringi]{stri_locate_all}}
#' @export
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex

#' @title get_name_from_path_chr
#' @description FUNCTION_DESCRIPTION
#' @param path_str PARAM_DESCRIPTION
#' @param with_ext PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[stringi]{stri_locate_all}}
#' @rdname get_name_from_path_chr
#' @export
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
get_name_from_path_chr <- function(path_str,
                                                with_ext = TRUE){
  if(with_ext){
    stringr::str_sub(path_str,
                     start = stringi::stri_locate_last_regex(path_str, "/")[,2] %>%
                       as.vector() +1)
  }else{
    stringr::str_sub(path_str,
                     start = stringi::stri_locate_last_regex(path_str, "/")[,2] %>%
                       as.vector() +1,
                     end = stringi::stri_locate_last_regex(path_str, "\\.")[,2] %>%
                       as.vector() -1)
  }
}

