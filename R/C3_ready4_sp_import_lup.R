
setOldClass(c("ready4_sp_import_lup","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import., Default: make_pt_ready4_sp_import_lup()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname ready4_sp_import_lup
#' @export 

ready4_sp_import_lup <- function(x = make_pt_ready4_sp_import_lup()){ 
validate_ready4_sp_import_lup(make_new_ready4_sp_import_lup(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname make_new_ready4_sp_import_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_sp_import_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_sp_import_lup",setdiff(make_pt_ready4_sp_import_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param file_type PARAM_DESCRIPTION, Default: character(0)
#' @param file_name PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_ui PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_db_ui PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_file_ext PARAM_DESCRIPTION, Default: character(0)
#' @param data_repo_save_type PARAM_DESCRIPTION, Default: character(0)
#' @param local_file_src PARAM_DESCRIPTION, Default: character(0)
#' @param make_script_src PARAM_DESCRIPTION, Default: character(0)
#' @param download_url PARAM_DESCRIPTION, Default: character(0)
#' @param inc_file_main PARAM_DESCRIPTION, Default: character(0)
#' @param inc_files_to_rename PARAM_DESCRIPTION, Default: list()
#' @param new_names_for_inc_files PARAM_DESCRIPTION, Default: list()
#' @param name PARAM_DESCRIPTION, Default: character(0)
#' @param country PARAM_DESCRIPTION, Default: character(0)
#' @param area_type PARAM_DESCRIPTION, Default: character(0)
#' @param area_bound_yr PARAM_DESCRIPTION, Default: character(0)
#' @param region PARAM_DESCRIPTION, Default: character(0)
#' @param data_type PARAM_DESCRIPTION, Default: character(0)
#' @param main_feature PARAM_DESCRIPTION, Default: character(0)
#' @param year PARAM_DESCRIPTION, Default: character(0)
#' @param year_start PARAM_DESCRIPTION, Default: character(0)
#' @param year_end PARAM_DESCRIPTION, Default: character(0)
#' @param ... Additional arguments, Default: character(0)
#' @param add_boundaries PARAM_DESCRIPTION, Default: list()
#' @return A prototype for Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname make_pt_ready4_sp_import_lup
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_sp_import_lup <- function(file_type = character(0),
file_name = character(0),
data_repo = character(0),
data_repo_ui = character(0),
data_repo_db_ui = character(0),
data_repo_file_ext = character(0),
data_repo_save_type = character(0),
local_file_src = character(0),
make_script_src = character(0),
download_url = character(0),
inc_file_main = character(0),
inc_files_to_rename = list(),
new_names_for_inc_files = list(),
name = character(0),
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
add_boundaries = list()){ 
args_ls <- list(file_type = file_type,
file_name = file_name,
data_repo = data_repo,
data_repo_ui = data_repo_ui,
data_repo_db_ui = data_repo_db_ui,
data_repo_file_ext = data_repo_file_ext,
data_repo_save_type = data_repo_save_type,
local_file_src = local_file_src,
make_script_src = make_script_src,
download_url = download_url,
inc_file_main = inc_file_main,
inc_files_to_rename = inc_files_to_rename,
new_names_for_inc_files = new_names_for_inc_files,
name = name,
country = country,
area_type = area_type,
area_bound_yr = area_bound_yr,
region = region,
data_type = data_type,
main_feature = main_feature,
year = year,
year_start = year_start,
year_end = year_end,
uid = uid,
add_boundaries = add_boundaries) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname validate_ready4_sp_import_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_sp_import_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_sp_import_lup())],
names(make_pt_ready4_sp_import_lup())))!=length(names(make_pt_ready4_sp_import_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_sp_import_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_sp_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_sp_import_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_sp_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_sp_import_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname is_ready4_sp_import_lup
#' @export 

is_ready4_sp_import_lup <- function(x) inherits(validate_ready4_sp_import_lup(x), "ready4_sp_import_lup")
