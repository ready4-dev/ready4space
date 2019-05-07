#' #' @title
#' #' Select and rename variables for 0-29 year old males and females in a population tibble.
#' #'
#' #' @description
#' #' This function updates an inputted tibble of population data for a profiled region by subsetting
#' #' the data and renaming the variables.
#' #'
#' #' @family spatial functions.
#' #'
#' #' @details
#' #'
#' #' @param population_tib A tibble of population projection data for a profiled region
#' #' broken down by unit of resolution.
#' #'
#' #' @param year A String specifying the year to which data relates.
#' #'
#' #' @param also_include A String vector specifying the year to which data relates.
#' #'
#' #' @return
#' #' A tibble.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#' #' @rdname spatial_select_rename_age_sex
#' #' @export
#' #' @importFrom dplyr select one_of
#'
#' spatial_select_rename_age_sex <- function (population_tib,
#'                                            year,
#'                                            also_include,
#'                                            sub_div_unit){
#'   if(!is.null(sub_div_unit)){
#'     if(sub_div_unit == "New South Wales"){
#'       population_tib <-  population_tib %>% dplyr::rename_at(dplyr::vars(dplyr::starts_with("Persons")),
#'                                                              # .vars = names(population_tib)[startsWith(names(population_tib),"Persons")],
#'                                                              dplyr::funs(paste0("y",year,".",gsub("-",
#'                                                                                                   ".",
#'                                                                                                   .)))) %>%
#'         dplyr::rename_at(dplyr::vars(dplyr::ends_with("+")),
#'                          dplyr::funs(gsub("\\+",".pl",.)))
#'       divide_large_age_band <- function(x, large_band, divided_by) ifelse(is.na(x),large_band/divided_by,x)
#'       population_tib <- population_tib %>%
#'         dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".0.4"),
#'                                      dplyr::ends_with(".5.9"),
#'                                      dplyr::ends_with(".10.14")),
#'                          large_band = population_tib %>% dplyr::pull(!!rlang::sym(paste0("y",year,".Persons.0.14"))), ### RLANG !!
#'                          divided_by = 3,
#'                          .funs = list(~ divide_large_age_band))
#'       population_tib <- population_tib %>%
#'         dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".15.19"),
#'                                      dplyr::ends_with(".20.24"),
#'                                      dplyr::ends_with(".25.29"),
#'                                      dplyr::ends_with(".30.34"),
#'                                      dplyr::ends_with(".35.39"),
#'                                      dplyr::ends_with(".40.44")),
#'                          large_band = population_tib %>% dplyr::pull(!!rlang::sym(paste0("y",year,".Persons.15.44"))),
#'                          divided_by = 6,
#'                          .funs = list(~ divide_large_age_band))
#'       population_tib <- population_tib %>%
#'         dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".45.49"),
#'                                      dplyr::ends_with(".50.54"),
#'                                      dplyr::ends_with(".55.59"),
#'                                      dplyr::ends_with(".60.64")),
#'                          large_band = population_tib %>% dplyr::pull(!!rlang::sym(paste0("y",year,".Persons.45.64"))),
#'                          divided_by = 4,
#'                          .funs = list(~ divide_large_age_band))
#'       population_tib <- population_tib %>%
#'         dplyr::mutate_at(dplyr::vars(dplyr::ends_with(".65.69"),
#'                                      dplyr::ends_with(".70.74"),
#'                                      dplyr::ends_with(".75.79"),
#'                                      dplyr::ends_with(".80.84"),
#'                                      dplyr::ends_with(".85.pl")),
#'                          large_band = population_tib %>% dplyr::pull(!!rlang::sym(paste0("y",year,".Persons.65.pl"))),
#'                          divided_by = 5,
#'                          .funs = list(~ divide_large_age_band))
#'       population_tib <- population_tib %>%
#'         dplyr::select(-c(!!rlang::sym(paste0("y",year,".Persons.0.14")),
#'                          !!rlang::sym(paste0("y",year,".Persons.15.44")),
#'                          !!rlang::sym(paste0("y",year,".Persons.45.64")),
#'                          !!rlang::sym(paste0("y",year,".Persons.65.pl")),
#'                          year))
#'       population_tib <- population_tib %>%
#'         dplyr::mutate_if(is.numeric,
#'                          .funs = list(Female = ~ . * 0.5,
#'                                       Male = ~ . * 0.5)) %>%
#'         dplyr::rename_at(dplyr::vars(dplyr::ends_with("_Female")),
#'                          .funs = dplyr::funs(stringr::str_replace(.,"Persons","Female") %>%
#'                                                stringr::str_replace("_Female",""))) %>%
#'         dplyr::rename_at(dplyr::vars(dplyr::ends_with("_Male")),
#'                          .funs = dplyr::funs(stringr::str_replace(.,"Persons","Male") %>%
#'                                                stringr::str_replace("_Male",""))) %>%
#'         dplyr::select(-dplyr::contains("Persons"))
#'     }else{
#'       population_tib <- population_tib %>% dplyr::select(dplyr::one_of(also_include),
#'                       which(
#'                         grepl('Female|Male',
#'                                     names(population_tib)) &
#'                           grepl('0-4|5-9|10-14|15-19|20-24|25-29',
#'                                       names(population_tib))))
#'         population_tib <- population_tib %>%
#'         dplyr::select(-which(grepl('Female|Male',
#'                                                names(population_tib)) &
#'                                      grepl('40-44',
#'                                                  names(population_tib))))
#'       names(population_tib) <- names(population_tib) %>%
#'         gsub("\r\n",
#'                    "",
#'                    .) %>%
#'         gsub("Females",
#'                    paste0("y",
#'                           year,
#'                           ".Females"),#"y2016.Females",
#'                    .) %>%
#'         gsub("Males",
#'                    paste0("y",
#'                           year,
#'                           ".Males"),#"y2016.Males",
#'                    .)  %>%
#'         gsub(" ",
#'                    ".",
#'                    .) %>%
#'         gsub("-",
#'                    ".",
#'                    .)
#'     }
#'
#'   }
#' return(population_tib)
#' }
