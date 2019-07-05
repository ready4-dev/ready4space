#' @title import_data
#' @description FUNCTION_DESCRIPTION
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
#' @rdname import_data
#' @export

import_data <- function(x,
                        ...){
  UseMethod("import_data",x)
}
#' @title download_data
#' @description FUNCTION_DESCRIPTION
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
#' @rdname import_data
#' @export

download_data <- function(x,
                        ...){
  UseMethod("download_data",x)
}
#' @title save_raw
#' @description FUNCTION_DESCRIPTION
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
#' @rdname import_data
#' @export

save_raw <- function(x,
                          ...){
  UseMethod("save_raw",x)
}

#' @title save_raw
#' @description FUNCTION_DESCRIPTION
#' @param required_data PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
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
#'  \code{\link[purrr]{map}}
#' @rdname save_raw
#' @export
#' @importFrom purrr walk
save_raw.ready4_sp_import_lup <- function(x,
                                          required_data,
                                          destination_directory){
  purrr::walk(required_data,
              ~ download_data(x = x,
                              destination_directory = destination_directory,
                              data_lookup_ref = .x))
}

#' @title download_data.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION, Default: 'name'
#' @param directory_sub_divs PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4s3]{ready4_sp_import_lup}}
#' @rdname download_data.ready4_sp_import_lup
#' @export
#' @importFrom ready4s3 ready4_sp_import_lup
download_data.ready4_sp_import_lup <- function(x,
                                            destination_directory,
                                            #data_import_lookup_tb,
                                            data_lookup_ref,
                                            lookup_variable = "name",
                                            directory_sub_divs = NULL){

  if(is.null(directory_sub_divs))
    directory_sub_divs <- names(ready4s3::ready4_sp_import_lup())[2:7]
  directory_paths <- data_import_get_dir_paths(x = x,
                                               destination_directory = destination_directory,
                                               data_lookup_ref = data_lookup_ref,
                                               lookup_variable = lookup_variable,
                                               directory_sub_divs = directory_sub_divs)
  data_import_make_directories(directory_paths = directory_paths)
  data_import_save_files(x = x,
                         data_lookup_ref = data_lookup_ref,
                         lookup_variable = lookup_variable,
                         directory_path = directory_paths[length(directory_paths)])


}
#' @title import_data.ready4_sp_import_lup
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param included_items_names PARAM_DESCRIPTION
#' @param item_data_type PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[sf]{st_read}}
#'  \code{\link[stats]{setNames}}
#' @rdname import_data.ready4_sp_import_lup
#' @export
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_chr map
#' @importFrom sf st_read
#' @importFrom stats setNames
import_data.ready4_sp_import_lup <- function(x, # data_import_items
                                             included_items_names,
                                             item_data_type,
                                             data_directory){
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
                                                          dplyr::select(c(name, country, area_type, region, data_type, main_feature, year, inc_file_main)),
                                                        # data_import_show_menu_of_type_detail(item_data_type,
                                                        #                                      x = x),
                                                        lookup_reference = .x,
                                                        data_directory = data_directory))
  if(item_data_type=="Shape"){
    item_list <- purrr::map(path_vec,
                            ~ sf::st_read(dsn=.x,
                                          layer = data_import_get_file_name_from_path(.x,
                                                                                      with_ext = FALSE))) %>%
      stats::setNames(included_items_names)
  }else{
    item_list <- purrr::map(path_vec,
                            ~ data_import_non_shape_items(.x,
                                                          x = downloaded_data_tb
                                                          # x
                            )) %>%
      stats::setNames(included_items_names)
  }
  return(item_list)
}

#' @title data_import_get_dir_paths
#' @description FUNCTION_DESCRIPTION
#' @param destination_directory PARAM_DESCRIPTION
#' @param x PARAM_DESCRIPTION
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
#' @rdname data_import_get_dir_paths
#' @export
#' @importFrom purrr map_chr accumulate
data_import_get_dir_paths <- function(x,
                                      destination_directory,
                                      data_lookup_ref,
                                      lookup_variable,
                                      directory_sub_divs){
  directory_names <- purrr::map_chr(directory_sub_divs,
                                    ~ data_get(data_lookup_tb = x,
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
#'  \code{\link[utils]{download.file}},\code{\link[utils]{unzip}}
#' @rdname data_import_save_files
#' @export
#' @importFrom purrr map_chr
#' @importFrom utils download.file unzip
data_import_save_files <- function(x,
                                   data_lookup_ref,
                                   lookup_variable,
                                   directory_path){
  download_components_vec <- purrr::map_chr(c("file_name",
                                              "file_type",
                                              "download_url",
                                              "inc_file_main",
                                              "local_file_src"),
                                            ~ data_get(data_lookup_tb = x,
                                                       lookup_reference = data_lookup_ref,
                                                       lookup_variable = lookup_variable,
                                                       target_variable = .x,
                                                       evaluate = FALSE))
  dest_file <- paste0(directory_path,
                      "/",
                      download_components_vec[1],
                      download_components_vec[2])
  if(!is.na(download_components_vec)){
    file.copy(from = download_components_vec[5],to = dest_file)
  }else{
    if(!is.na(paste0(directory_path,
                     "/",
                     download_components_vec[4]))){
      if(!file.exists(paste0(directory_path,
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
                                 directory_path = directory_path)
      }
    }
  }
}
#' @title data_import_rename_files
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param data_lookup_ref PARAM_DESCRIPTION
#' @param lookup_variable PARAM_DESCRIPTION
#' @param directory_path PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map2}}
#' @rdname data_import_rename_files
#' @export
#' @importFrom purrr walk2
data_import_rename_files <- function(x,
                                     data_lookup_ref,
                                     lookup_variable,
                                     directory_path){
  old_names_list <- data_get(data_lookup_tb = x,
                             lookup_reference = data_lookup_ref,
                             lookup_variable = lookup_variable,
                             target_variable = "inc_files_to_rename",
                             evaluate = FALSE)
  new_names_list <- data_get(data_lookup_tb = x,
                             lookup_reference = data_lookup_ref,
                             lookup_variable = lookup_variable,
                             target_variable = "new_names_for_inc_files",
                             evaluate = FALSE)
  if(!is.na(old_names_list)){
    purrr::walk2(old_names_list,
                 new_names_list,
                 ~ file.rename(paste0(directory_path,
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
    dplyr::select(c(1:7,11))
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
#' @rdname data_import_get_one_path
#' @export
#' @importFrom purrr map_chr
#' @importFrom dplyr select
data_import_get_one_path <- function(downloaded_data_tb,
                                     lookup_reference,
                                     data_directory) {
  path_element_vector <- purrr::map_chr(downloaded_data_tb %>% dplyr::select(-name) %>% names(),
                                        ~ data_get(data_lookup_tb = downloaded_data_tb,
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
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_c}}
#'  \code{\link[stringi]{stri_locate_all}}
#'  \code{\link[purrr]{map}}
#'  \code{\link[tibble]{as_tibble}},\code{\link[tibble]{deprecated}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{filter}}
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[stats]{setNames}}
#' @rdname data_import_non_shape_items
#' @export
#' @importFrom stringr str_sub str_c
#' @importFrom stringi stri_locate_last_regex
#' @importFrom purrr map_chr map
#' @importFrom tibble as.tibble
#' @importFrom dplyr mutate rename filter
#' @importFrom readxl read_excel
#' @importFrom stats setNames
data_import_non_shape_items <- function(path_str,
                                        x){
  file_name <-  data_import_get_file_name_from_path(path_str)
  file_ext <- file_name %>% stringr::str_sub(start = stringi::stri_locate_last_regex(file_name, "\\.")[,2] %>%
                                               as.vector())
  data_type <- data_get(data_lookup_tb = x,#data_import_show_menu_detail(x = x),
                        lookup_reference = file_name,
                        lookup_variable = "inc_file_main",
                        target_variable = "data_type",
                        evaluate = FALSE)
  var_name_vec <- c("area_type",
                    "main_feature",
                    "year",
                    "region")
  var_val_vec <- purrr::map_chr(var_name_vec,
                                  ~ data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
                                                                                                   x = x),
                                                         lookup_reference = file_name,
                                                         lookup_variable = "inc_file_main",
                                                         target_variable = .x,
                                                         evaluate = FALSE))
  data_feature <- data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
                                                                                 x = x),
                           lookup_reference = file_name,
                           lookup_variable = "inc_file_main",
                           target_variable = "main_feature",
                           evaluate = FALSE) %>%
    stringr::str_sub(1,3)
  area_type <- data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
                                                                              x = x),
                                    lookup_reference = file_name,
                                    lookup_variable = "inc_file_main",
                                    target_variable = "area_type",
                                    evaluate = FALSE)
  make_import_obj_for_context(context = data_get(data_lookup_tb = data_import_show_menu_of_type_detail(data_type,
                                                                                                       x = x),
                                                 lookup_reference = file_name,
                                                 lookup_variable = "inc_file_main",
                                                 target_variable = "country",
                                                 evaluate = FALSE),
                              var_val_vec = var_val_vec,
                              path_str = path_str)
}

make_import_obj_for_context <- function(context,
                                        var_val_vec,
                                        path_str){
  ## Replace condition logic by class based methods
  if(context == "Australia")
    make_import_obj_for_australia(var_val_vec = var_val_vec,
                                  path_str = path_str)
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

#' @title data_import_get_file_name_from_path
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
#' @rdname data_import_get_file_name_from_path
#' @export
#' @importFrom stringr str_sub
#' @importFrom stringi stri_locate_last_regex
data_import_get_file_name_from_path <- function(path_str,
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

