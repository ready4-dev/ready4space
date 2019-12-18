#' ready4_sp_import_lup
#' @name ready4_sp_import_lup
#' @description Create a new valid instance of the S3 class: ready4_sp_import_lup
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_sp_import_lup()
#' @return A validated instance of the ready4_sp_import_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of sources of raw (un-processed) spatial data to import.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_sp_import_lup
#' @export 

ready4_sp_import_lup <- function(x = make_prototype_ready4_sp_import_lup()){ 
validate_ready4_sp_import_lup(new_ready4_sp_import_lup(x))
}
#' ready4_sp_import_lup
#' @name new_ready4_sp_import_lup
#' @description Create a new unvalidated instance of the S3 class: new_ready4_sp_import_lup
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_sp_import_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{is_tibble}}
#' @rdname ready4_sp_import_lup
#' @export 
#' @importFrom tibble is_tibble
new_ready4_sp_import_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append("ready4_sp_import_lup",
class(x))
x
}
#' ready4_sp_import_lup
#' @name make_prototype_ready4_sp_import_lup
#' @description Create a new prototype for S3 class: make_prototype_ready4_sp_import_lup

#' @return A prototpe for ready4_sp_import_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{reduce}}
#'  \code{\link[tibble]{tibble}}
#'  \code{\link[ready4use]{ready4_all_import_lup}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[rlang]{sym}}
#' @rdname ready4_sp_import_lup
#' @export 
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @importFrom ready4use ready4_all_import_lup
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
make_prototype_ready4_sp_import_lup <- function(){ 
purrr::reduce(names(tibble::tibble(name = character(0),
country = character(0),
area_type = character(0),
area_bound_yr = character(0),
region = character(0),
data_type = character(0),
main_feature = character(0),
year = character(0),
year_start = character(0),
year_end = character(0),
uid = character(0),
add_boundaries = list())),
.init = ready4use::ready4_all_import_lup(),
 ~ .x %>% dplyr::mutate(!!rlang::sym(.y) := eval(parse(text=tibble::tibble(name = character(0),
country = character(0),
area_type = character(0),
area_bound_yr = character(0),
region = character(0),
data_type = character(0),
main_feature = character(0),
year = character(0),
year_start = character(0),
year_end = character(0),
uid = character(0),
add_boundaries = list())[.y]))))%>% dplyr::select(c(name,country,area_type,area_bound_yr,region,data_type,main_feature,year,year_start,year_end,uid,add_boundaries,local_file_src,make_script_src,download_url,inc_file_main,inc_files_to_rename,new_names_for_inc_files,file_type,file_name,data_repo,data_repo_ui,data_repo_db_ui,data_repo_file_ext,data_repo_save_type))
}
#' ready4_sp_import_lup
#' @name validate_ready4_sp_import_lup
#' @description Validate an instance of the S3 class: validate_ready4_sp_import_lup
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_sp_import_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_detect}},\code{\link[stringr]{str_c}}
#'  \code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#'  \code{\link[tidyr]{gather}}
#'  \code{\link[purrr]{map2}}
#' @rdname ready4_sp_import_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_sp_import_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_sp_import_lup())],
names(make_prototype_ready4_sp_import_lup())))!=length(names(make_prototype_ready4_sp_import_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_sp_import_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_sp_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_sp_import_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_sp_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_sp_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' ready4_sp_import_lup
#' @name is_ready4_sp_import_lup
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_sp_import_lup
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_sp_import_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_sp_import_lup
#' @export 

is_ready4_sp_import_lup <- function(x) inherits(validate_ready4_sp_import_lup(x), "ready4_sp_import_lup")
