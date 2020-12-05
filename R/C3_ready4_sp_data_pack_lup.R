
setOldClass(c("ready4_sp_data_pack_lup","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data)., Default: make_pt_ready4_sp_data_pack_lup()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname ready4_sp_data_pack_lup
#' @export 

ready4_sp_data_pack_lup <- function(x = make_pt_ready4_sp_data_pack_lup()){ 
validate_ready4_sp_data_pack_lup(make_new_ready4_sp_data_pack_lup(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname make_new_ready4_sp_data_pack_lup
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4_sp_data_pack_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_sp_data_pack_lup",setdiff(make_pt_ready4_sp_data_pack_lup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
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
#' @param source_reference PARAM_DESCRIPTION, Default: character(0)
#' @param additional_detail PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname make_pt_ready4_sp_data_pack_lup
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4_sp_data_pack_lup <- function(name = character(0),
country = character(0),
area_type = character(0),
area_bound_yr = character(0),
region = character(0),
data_type = character(0),
main_feature = character(0),
year = character(0),
year_start = character(0),
year_end = character(0),
source_reference = character(0),
additional_detail = character(0)){ 
args_ls <- list(name = name,
country = country,
area_type = area_type,
area_bound_yr = area_bound_yr,
region = region,
data_type = data_type,
main_feature = main_feature,
year = year,
year_start = year_start,
year_end = year_end,
source_reference = source_reference,
additional_detail = additional_detail) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname validate_ready4_sp_data_pack_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_sp_data_pack_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4_sp_data_pack_lup())],
names(make_pt_ready4_sp_data_pack_lup())))!=length(names(make_pt_ready4_sp_data_pack_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4_sp_data_pack_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_ready4_sp_data_pack_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4_sp_data_pack_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4_sp_data_pack_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4_sp_data_pack_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @details Readyforwhatsnext S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @rdname is_ready4_sp_data_pack_lup
#' @export 

is_ready4_sp_data_pack_lup <- function(x) inherits(validate_ready4_sp_data_pack_lup(x), "ready4_sp_data_pack_lup")
