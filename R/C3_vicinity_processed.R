
setOldClass(c("vicinity_processed","tbl_df", "tbl", "data.frame"))
#' ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Create a new valid instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x A prototype for the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data)., Default: make_pt_vicinity_processed()
#' @return A validated instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details ready4 submodule for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname vicinity_processed
#' @export 
vicinity_processed <- function(x = make_pt_vicinity_processed()){ 
validate_vicinity_processed(make_new_vicinity_processed(x))
}
#' make new vicinity processed ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Create a new unvalidated instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x A prototype for the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @return An unvalidated instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details ready4 submodule for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname make_new_vicinity_processed
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_vicinity_processed <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_processed",setdiff(make_pt_vicinity_processed() %>% class(),class(x))),
class(x))
x
}
#' make prototype vicinity processed ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
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
#' @param source_reference_chr Source reference (a character vector), Default: character(0)
#' @param additional_detail_chr Additional detail (a character vector), Default: character(0)
#' @return A prototype for ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' 
#' @rdname vicinity_processed
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_processed <- function(name_chr = character(0),
country_chr = character(0),
area_type_chr = character(0),
area_bndy_yr_chr = character(0),
region_chr = character(0),
data_type_chr = character(0),
main_feature_chr = character(0),
year_chr = character(0),
year_start_chr = character(0),
year_end_chr = character(0),
source_reference_chr = character(0),
additional_detail_chr = character(0)){ 
args_ls <- list(name_chr = name_chr,
country_chr = country_chr,
area_type_chr = area_type_chr,
area_bndy_yr_chr = area_bndy_yr_chr,
region_chr = region_chr,
data_type_chr = data_type_chr,
main_feature_chr = main_feature_chr,
year_chr = year_chr,
year_start_chr = year_start_chr,
year_end_chr = year_end_chr,
source_reference_chr = source_reference_chr,
additional_detail_chr = additional_detail_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate vicinity processed ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Validate an instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x An unvalidated instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @return A prototpe for ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details ready4 submodule for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname validate_vicinity_processed
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_vicinity_processed <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_processed())],
names(make_pt_vicinity_processed())))!=length(names(make_pt_vicinity_processed()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_processed()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_vicinity_processed() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_vicinity_processed())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_vicinity_processed() %>% 
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
#' is vicinity processed ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 submodule class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' 
#' @rdname vicinity_processed
#' @export 
is_vicinity_processed <- function(x) inherits(validate_vicinity_processed(x), "vicinity_processed")
