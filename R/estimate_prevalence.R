#' estimate_prevalence
#' Function to estimate the prevalence of a specified condition, for a specified disorder for a specified area / year.
#' @param pop_data PARAM_DESCRIPTION
#' @param prev_rates_vec PARAM_DESCRIPTION
##' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{funs}}
#'  \code{\link[purrr]{map2}},\code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[tibble]{tibble}}
#' @rdname estimate_prevalence
#' @export
#' @importFrom dplyr select starts_with mutate summarise_at vars contains funs
#' @importFrom purrr map2_dbl map_lgl reduce
#' @importFrom stringr str_sub
#' @importFrom stats setNames
#' @importFrom rlang sym
#' @importFrom tibble tibble

estimate_prevalence <- function(pop_data,
                                prev_rates_vec){
  pop_totals_vec <- pop_data %>% names()
  pop_totals_vec <- pop_totals_vec[pop_totals_vec %>% startsWith(prefix="tx_")]
  pop_totals_vec <- pop_totals_vec[purrr::map_lgl(pop_totals_vec,
                                                 ~ .x %>% stringr::str_sub(start = -4) %in% names(prev_rates_vec))]
  prev_summary <- purrr::reduce(1:length(pop_totals_vec),
                                .init = pop_data,
                                ~ .x %>%
                                  dplyr::mutate(!!rlang::sym(paste0(names(prev_rates_vec)[.y],"_prev")) := !!rlang::sym(pop_totals_vec[.y]) * prev_rates_vec[.y]))
  prev_area_summary <- prev_summary %>%
    dplyr::summarise_at(dplyr::vars(dplyr::contains("_prev")),
                        dplyr::funs(sum))
  st_geometry(prev_area_summary) <- NULL
  prev_area_sum_vec <- prev_area_summary %>%
    unlist()
  female_prev_vec <- prev_area_sum_vec[startsWith(names(prev_area_sum_vec),"f_")]
  male_prev_vec <- prev_area_sum_vec[startsWith(names(prev_area_sum_vec),"m_")]
  ages <- names(female_prev_vec) %>% stringr::str_sub(start=-7,end=-6)
  summ_tb <- tibble::tibble(age = ages,
                            Females = female_prev_vec,
                            Males = male_prev_vec) %>%
    dplyr::mutate(Total = Females + Males)
  return(summ_tb)
}

#' make_prev_struc_par_tb
#' FUNCTION_DESCRIPTION
#' @param disorder PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @param ages PARAM_DESCRIPTION
#' @param sexes PARAM_DESCRIPTION
#' @param pref_source PARAM_DESCRIPTION
#' @param prev_rates PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[stats]{setNames}}
#' @rdname make_prev_struc_par_tb
#' @export
#' @importFrom dplyr select starts_with
#' @importFrom purrr map2_dbl
#' @importFrom stringr str_sub
#' @importFrom stats setNames

make_prev_struc_par_tb <- function(disorder,
                                   period,
                                   ages,
                                   sexes,
                                   pref_source,
                                   prev_rates){
  age_sex_source_tb <- pref_sources_for_age_range(disorder = disorder,
                                                  period = period,
                                                  ages =  ages,
                                                  sexes = sexes,
                                                  pref_source = pref_source)
  age_sex_source_tb <- age_sex_source_tb %>%
    dplyr::select(dplyr::starts_with("Female_"),
                  dplyr::starts_with(("Male_")))
  age_sex_vec  <- age_sex_source_tb %>%
    names()
  source_vec <- age_sex_source_tb %>%
    unlist() %>%
    as.vector()
  prev_rates_vec <- purrr::map2_dbl(age_sex_vec,
                                    source_vec,
                                    ~ pick_rate_from_source(disorder,
                                                            period,
                                                            source=.y,
                                                            age=.x %>%
                                                              stringr::str_sub(start=-2) %>% #### HUH?
                                                              as.numeric(),
                                                            sex=.x %>%
                                                              stringr::str_sub(end=-4), #### HUH?
                                                            prev_rates = prev_rates)) %>%
    stats::setNames(age_sex_vec %>%
                      gsub("Female","f",.)%>%
                      gsub("Male","m",.))
  return(prev_rates_vec)
}

#' pick_rate_from_source
#' FUNCTION_DESCRIPTION
#' @param disorder PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @param source PARAM_DESCRIPTION
#' @param age PARAM_DESCRIPTION
#' @param sex PARAM_DESCRIPTION
#' @param prev_rates PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pick_rate_from_source
#' @export
#' @importFrom dplyr filter select pull

pick_rate_from_source <-function(disorder,
                                 period,
                                 source,
                                 age,
                                 sex,
                                 prev_rates){
  look_up <- paste0(sex,"_",age)
  sel_rate <- prev_rates  %>%
    dplyr::filter(Disorder==disorder,
                  Period==period,
                  Source==source) %>%
    dplyr::select(look_up) %>%
    dplyr::pull()
  return(sel_rate)
}

#' pick_source
#' FUNCTION_DESCRIPTION
#' @param disorder PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @param age PARAM_DESCRIPTION
#' @param sex PARAM_DESCRIPTION
#' @param pref_source PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pick_source
#' @export
#' @importFrom dplyr filter select pull
#' @importFrom purrr pluck

pick_source <- function(disorder,
                        period,
                        age,
                        sex,
                        pref_source){
  look_up <- paste0(sex,"_",age)
  sel_source <- pref_source  %>%
    dplyr::filter(Disorder==disorder, Period==period) %>%
    dplyr::select(look_up) %>%
    dplyr::pull() %>% na.omit %>%
    purrr::pluck(1)
  return(sel_source)
}

#' pref_sources_for_age_rang
#' FUNCTION_DESCRIPTION
#' @param disorder PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @param ages PARAM_DESCRIPTION
#' @param sexes PARAM_DESCRIPTION
#' @param pref_source PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pref_sources_for_age_range
#' @export
#' @importFrom dplyr select filter mutate
#' @importFrom purrr map_chr map flatten_chr reduce prepend
#' @importFrom stringr str_sub

pref_sources_for_age_range <- function(disorder,
                                       period,
                                       ages,
                                       sexes,
                                       pref_source){
  base_tib <- pref_source %>% dplyr::select(Disorder, Period) %>% dplyr::filter(Disorder == disorder,
                                                                                Period == period)
  add_each_age <- function(ages,
                           sex){purrr::map_chr(ages,
                                               ~ paste0(sex,"_",.))}
  new_cols <- purrr::map(sexes,
                         ~ add_each_age(ages = ages, sex = .)) %>% purrr::flatten_chr()

  purrr::reduce(purrr::prepend(new_cols,
                               list(a=base_tib)),
                .f = function(x,y) x %>%
                  dplyr::mutate(!!y := pick_source(disorder = disorder,
                                                   period = period,
                                                   age = stringr::str_sub(y,-2),
                                                   sex = stringr::str_sub(y,1,-4),
                                                   pref_source = pref_source)))
}
