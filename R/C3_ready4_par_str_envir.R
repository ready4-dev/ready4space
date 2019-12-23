
#' ready4_par_str_envir
#' @name ready4_par_str_envir
#' @description Create a new valid instance of the S3 class: ready4_par_str_envir
#' @param x PARAM_DESCRIPTION, Default: make_prototype_ready4_par_str_envir()
#' @return A validated instance of the ready4_par_str_envir class
#' @details Readyforwhatsnext S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_par_str_envir
#' @export 

ready4_par_str_envir <- function(x = make_prototype_ready4_par_str_envir()){ 
validate_ready4_par_str_envir(new_ready4_par_str_envir(x))
}
#' ready4_par_str_envir
#' @name new_ready4_par_str_envir
#' @description Create a new unvalidated instance of the S3 class: new_ready4_par_str_envir
#' @param x PARAM_DESCRIPTION
#' @return An unvalidated instance of the ready4_par_str_envir class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{is_tibble}}
#' @rdname ready4_par_str_envir
#' @export 
#' @importFrom tibble is_tibble
new_ready4_par_str_envir <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4_par_str_envir",setdiff(make_prototype_ready4_par_str_envir() %>% class(),class(x))),
class(x))
x
}
#' ready4_par_str_envir
#' @name make_prototype_ready4_par_str_envir
#' @description Create a new prototype for S3 class: make_prototype_ready4_par_str_envir

#' @return A prototpe for ready4_par_str_envir class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[tibble]{tibble}}
#' @rdname ready4_par_str_envir
#' @export 
#' @importFrom tibble tibble
make_prototype_ready4_par_str_envir <- function(){ 
tibble::tibble(param_name = character(0),
deter_val = numeric(0),
distribution = character(0),
dist_param_1 = numeric(0),
dist_param_2 = numeric(0),
dist_param_3 = numeric(0),
dist_param_4 = numeric(0),
transformation = character(0),
use_in = character(0),
source = character(0))
}
#' ready4_par_str_envir
#' @name validate_ready4_par_str_envir
#' @description Validate an instance of the S3 class: validate_ready4_par_str_envir
#' @param x PARAM_DESCRIPTION
#' @return A prototpe for ready4_par_str_envir class
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
#' @rdname ready4_par_str_envir
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4_par_str_envir <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_prototype_ready4_par_str_envir())],
names(make_prototype_ready4_par_str_envir())))!=length(names(make_prototype_ready4_par_str_envir()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_prototype_ready4_par_str_envir()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
 if(!identical(make_prototype_ready4_par_str_envir() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_prototype_ready4_par_str_envir())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_prototype_ready4_par_str_envir() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_prototype_ready4_par_str_envir() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}
x}
#' ready4_par_str_envir
#' @name is_ready4_par_str_envir
#' @description Check whether an object is a valid instance of the S3 class: is_ready4_par_str_envir
#' @param x PARAM_DESCRIPTION
#' @return A logical value, TRUE if a valid instance of the ready4_par_str_envir class
#' 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ready4_par_str_envir
#' @export 

is_ready4_par_str_envir <- function(x) inherits(validate_ready4_par_str_envir(x), "ready4_par_str_envir")
