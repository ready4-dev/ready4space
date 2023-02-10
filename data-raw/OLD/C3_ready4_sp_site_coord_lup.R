
setOldClass(c("vicinity_points","tbl_df", "tbl", "data.frame"))
#' Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Create a new valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes., Default: make_pt_vicinity_points()
#' @return A validated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname vicinity_points
#' @export

vicinity_points <- function(x = make_pt_vicinity_points()){
validate_vicinity_points(make_new_vicinity_points(x))
}
#' Make new Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Create a new unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x A prototype for the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @return An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname make_new_vicinity_points
#' @export
#' @importFrom tibble is_tibble
make_new_vicinity_points <- function(x){
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("vicinity_points",setdiff(make_pt_vicinity_points() %>% class(),class(x))),
class(x))
x
}
#' Make prototype Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Create a new prototype for the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param service_type PARAM_DESCRIPTION, Default: character(0)
#' @param cluster_name PARAM_DESCRIPTION, Default: character(0)
#' @param service_name PARAM_DESCRIPTION, Default: character(0)
#' @param ... Additional arguments, Default: numeric(0)
#' @param long PARAM_DESCRIPTION, Default: numeric(0)
#' @return A prototype for Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname make_pt_vicinity_points
#' @export
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_vicinity_points <- function(service_type = character(0),
cluster_name = character(0),
service_name = character(0),
lat = numeric(0),
long = numeric(0)){
args_ls <- list(service_type = service_type,
cluster_name = cluster_name,
service_name = service_name,
lat = lat,
long = long) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Validate an instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x An unvalidated instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @return A prototpe for Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname validate_vicinity_points
#' @export
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_vicinity_points <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_vicinity_points())],
names(make_pt_vicinity_points())))!=length(names(make_pt_vicinity_points()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_vicinity_points()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_pt_vicinity_points() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::arrange(variable),
x %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::filter(variable %in% names(make_pt_vicinity_points())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_vicinity_points() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::pull(1),
 make_pt_vicinity_points() %>%
dplyr::summarise_all(class) %>%
 tidyr::gather(variable,class) %>%
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>%
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' Is Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @description Check whether an object is a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @rdname is_vicinity_points
#' @export

is_vicinity_points <- function(x) inherits(validate_vicinity_points(x), "vicinity_points")
