
#' ready4_sp_resolution_lup
#' @name ready4_sp_resolution_lup
#' @description Create a new valid instance of the S3 class: ready4_sp_resolution_lup
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_sp_resolution_lup()
#' @return A validated instance of the ready4_sp_resolution_lup class
#' @details Readyforwhatsnext S3 class for tibble object lookup table of the relative resolutions of different spatial objects.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_sp_resolution_lup
#' @export 

ready4_sp_resolution_lup <- function(x = make_prototype_ready4_sp_resolution_lup()){ 
validate_ready4_sp_resolution_lup(new_ready4_sp_resolution_lup(x))
}
#' ready4_sp_resolution_lup
#' @name new_ready4_sp_resolution_lup
#' @description Create a new unvalidated instance of the S3 class: new_ready4_sp_resolution_lup
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_sp_resolution_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{is_tibble}}
#' @rdname ready4_sp_resolution_lup
#' @export 
#' @importFrom tibble is_tibble
new_ready4_sp_resolution_lup <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_sp_resolution_lup",setdiff(make_prototype_ready4_sp_resolution_lup() %>% class(),class(x))),
class(x))
x
}
#' ready4_sp_resolution_lup
#' @name make_prototype_ready4_sp_resolution_lup
#' @description Create a new prototype for S3 class: make_prototype_ready4_sp_resolution_lup

#' @return A prototpe for ready4_sp_resolution_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{tibble}}
#' @rdname ready4_sp_resolution_lup
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_sp_resolution_lup <- function(){ 
tibble::tibble(parent_area = character(0),
boundary_year = numeric(0),
area_type = character(0),
area_count = numeric(0),
complete = logical(0),
summed_area = numeric(0),
mean_size = numeric(0))
}
#' ready4_sp_resolution_lup
#' @name validate_ready4_sp_resolution_lup
#' @description Validate an instance of the S3 class: validate_ready4_sp_resolution_lup
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_sp_resolution_lup class
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
#' @rdname ready4_sp_resolution_lup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_sp_resolution_lup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_sp_resolution_lup())],
names(make_prototype_ready4_sp_resolution_lup())))!=length(names(make_prototype_ready4_sp_resolution_lup()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_sp_resolution_lup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_sp_resolution_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_sp_resolution_lup())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_sp_resolution_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_sp_resolution_lup() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' ready4_sp_resolution_lup
#' @name is_ready4_sp_resolution_lup
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_sp_resolution_lup
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_sp_resolution_lup class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_sp_resolution_lup
#' @export 

is_ready4_sp_resolution_lup <- function(x) inherits(validate_ready4_sp_resolution_lup(x), "ready4_sp_resolution_lup")
