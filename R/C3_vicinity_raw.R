
setOldClass(c("vicinity_raw","tbl_df", "tbl", "data.frame"))
#' ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Create a new valid instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x A prototype for the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import., Default: make_pt_vicinity_raw()
#' @return A validated instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details ready4 submodule for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname vicinity_raw
#' @export 
vicinity_raw <- function(x = make_pt_vicinity_raw()){ 
validate_vicinity_raw(make_new_vicinity_raw(x))
}
#' make new vicinity raw ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Create a new unvalidated instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x A prototype for the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @return An unvalidated instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details ready4 submodule for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname make_new_vicinity_raw
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_raw <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_raw",setdiff(make_pt_vicinity_raw() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity raw ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param file_type_chr File type (a character vector), Default: character(0)
#' @param file_name_chr File name (a character vector), Default: character(0)
#' @param data_repo_chr Data repository (a character vector), Default: character(0)
#' @param data_repo_ui_chr Data repository user interface (a character vector), Default: character(0)
#' @param data_repo_db_ui_chr Data repository database user interface (a character vector), Default: character(0)
#' @param data_repo_file_ext_chr Data repository file extension (a character vector), Default: character(0)
#' @param data_repo_save_type_chr Data repository save type (a character vector), Default: character(0)
#' @param local_file_src_chr Local file source (a character vector), Default: character(0)
#' @param path_to_make_script_chr Path to make script (a character vector), Default: character(0)
#' @param download_url_chr Download url (a character vector), Default: character(0)
#' @param inc_file_main_chr Include file main (a character vector), Default: character(0)
#' @param inc_fls_to_rename_ls Include files to rename (a list), Default: list()
#' @param new_nms_for_inc_fls_ls New names for include files (a list), Default: list()
#' @param name_chr Name (a character vector), Default: character(0)
#' @param country_chr Country (a character vector), Default: character(0)
#' @param area_type_chr Area type (a character vector), Default: character(0)
#' @param area_bndy_yr_chr Area boundary year (a character vector), Default: character(0)
#' @param region_chr Region (a character vector), Default: character(0)
#' @param data_type_chr Data type (a character vector), Default: character(0)
#' @param main_feature_chr Main feature (a character vector), Default: character(0)
#' @param year_chr Year (a character vector), Default: character(0)
#' @param year_start_chr Year start (a character vector), Default: character(0)
#' @param year_end_chr Year end (a character vector), Default: character(0)
#' @param uid_chr Unique identifier (a character vector), Default: character(0)
#' @param add_bndys_from_ls Add boundaries from (a list), Default: list()
#' @return A prototype for ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' 
#' @rdname vicinity_raw
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_raw <- function(file_type_chr = character(0),
file_name_chr = character(0),
data_repo_chr = character(0),
data_repo_ui_chr = character(0),
data_repo_db_ui_chr = character(0),
data_repo_file_ext_chr = character(0),
data_repo_save_type_chr = character(0),
local_file_src_chr = character(0),
path_to_make_script_chr = character(0),
download_url_chr = character(0),
inc_file_main_chr = character(0),
inc_fls_to_rename_ls = list(),
new_nms_for_inc_fls_ls = list(),
name_chr = character(0),
country_chr = character(0),
area_type_chr = character(0),
area_bndy_yr_chr = character(0),
region_chr = character(0),
data_type_chr = character(0),
main_feature_chr = character(0),
year_chr = character(0),
year_start_chr = character(0),
year_end_chr = character(0),
uid_chr = character(0),
add_bndys_from_ls = list()){ 
args_ls <- list(file_type_chr = file_type_chr,
file_name_chr = file_name_chr,
data_repo_chr = data_repo_chr,
data_repo_ui_chr = data_repo_ui_chr,
data_repo_db_ui_chr = data_repo_db_ui_chr,
data_repo_file_ext_chr = data_repo_file_ext_chr,
data_repo_save_type_chr = data_repo_save_type_chr,
local_file_src_chr = local_file_src_chr,
path_to_make_script_chr = path_to_make_script_chr,
download_url_chr = download_url_chr,
inc_file_main_chr = inc_file_main_chr,
inc_fls_to_rename_ls = inc_fls_to_rename_ls,
new_nms_for_inc_fls_ls = new_nms_for_inc_fls_ls,
name_chr = name_chr,
country_chr = country_chr,
area_type_chr = area_type_chr,
area_bndy_yr_chr = area_bndy_yr_chr,
region_chr = region_chr,
data_type_chr = data_type_chr,
main_feature_chr = main_feature_chr,
year_chr = year_chr,
year_start_chr = year_start_chr,
year_end_chr = year_end_chr,
uid_chr = uid_chr,
add_bndys_from_ls = add_bndys_from_ls) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity raw ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @description Validate an instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x An unvalidated instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @return A prototpe for ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @details ready4 submodule for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @rdname validate_vicinity_raw
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_raw <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_raw())],
names(make_pt_vicinity_raw())))!=length(names(make_pt_vicinity_raw()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_raw()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_raw() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_raw())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_raw() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' is vicinity raw ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 submodule class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' 
#' @rdname vicinity_raw
#' @export 
is_vicinity_raw <- function(x) inherits(validate_vicinity_raw(x), "vicinity_raw")
