
setOldClass(c("vicinity_resolutions","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects., Default: make_pt_vicinity_resolutions()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname vicinity_resolutions
#' @export

vicinity_resolutions <- function(x = make_pt_vicinity_resolutions()){
validate_vicinity_resolutions(make_new_vicinity_resolutions(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname make_new_vicinity_resolutions
#' @export
#' @importFrom tibble is_tibble
make_new_vicinity_resolutions <- function(x){
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_resolutions",setdiff(make_pt_vicinity_resolutions() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param parent_area PARAM_DESCRIPTION, Default: character(0)
#' @param boundary_year PARAM_DESCRIPTION, Default: numeric(0)
#' @param area_type PARAM_DESCRIPTION, Default: character(0)
#' @param area_count PARAM_DESCRIPTION, Default: numeric(0)
#' @param complete PARAM_DESCRIPTION, Default: logical(0)
#' @param summed_area PARAM_DESCRIPTION, Default: numeric(0)
#' @param mean_size PARAM_DESCRIPTION, Default: numeric(0)
#' @return A prototype for Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname make_pt_vicinity_resolutions
#' @export
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_resolutions <- function(parent_area = character(0),
boundary_year = numeric(0),
area_type = character(0),
area_count = numeric(0),
complete = logical(0),
summed_area = numeric(0),
mean_size = numeric(0)){
args_ls <- list(parent_area = parent_area,
boundary_year = boundary_year,
area_type = area_type,
area_count = area_count,
complete = complete,
summed_area = summed_area,
mean_size = mean_size) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname validate_vicinity_resolutions
#' @export
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_vicinity_resolutions <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_resolutions())],
names(make_pt_vicinity_resolutions())))!=length(names(make_pt_vicinity_resolutions()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_resolutions()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_vicinity_resolutions() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::arrange(variable),
x %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::filter(variable %in% names(make_pt_vicinity_resolutions())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_vicinity_resolutions() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::pull(1),
 make_pt_vicinity_resolutions() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>%
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @rdname is_vicinity_resolutions
#' @export

is_vicinity_resolutions <- function(x) inherits(validate_vicinity_resolutions(x), "vicinity_resolutions")
