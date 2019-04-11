#' #' estimate_prevalence
#' #' Function to estimate the prevalence of a specified condition, for a specified disorder for a specified area / year.
#' #' @param sp_data_sf PARAM_DESCRIPTION
#' #' @param prev_rates_vec PARAM_DESCRIPTION
#' ##' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[dplyr]{select}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{funs}}
#' #'  \code{\link[purrr]{map2}},\code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#' #'  \code{\link[stringr]{str_sub}}
#' #'  \code{\link[stats]{setNames}}
#' #'  \code{\link[rlang]{sym}}
#' #'  \code{\link[tibble]{tibble}}
#' #' @rdname estimate_prevalence
#' #' @export
#' #' @importFrom dplyr select starts_with mutate summarise_at vars contains funs
#' #' @importFrom purrr map2_dbl map_lgl reduce
#' #' @importFrom stringr str_sub
#' #' @importFrom stats setNames
#' #' @importFrom rlang sym
#' #' @importFrom tibble tibble
#' 
#' estimate_prevalence <- function(sp_data_sf,
#'                                 sp_unit_id = "pop_sp_unit_id",
#'                                 #prev_rates_vec,
#'                                 param_tb,
#'                                 par_name_var = "parameter_name",
#'                                 it_nbr){
#'   param_tb_slimmed <- param_tb %>%
#'     dplyr::filter(startsWith(parameter_name,"prev")) %>%
#'     dplyr::select(!!par_name_var,
#'                   paste0("v_it_", it_nbr))
#'   #prefix <- param_tb_slimmed[1,1] %>% as.vector() %>% stringr::str_sub(end=10)
#'   col_names <- names(sp_data_sf)[(names(sp_data_sf)%>% startsWith(prefix = "tx_") |
#'                                    names(sp_data_sf)%>% startsWith(prefix = "t0_")) & !names(sp_data_sf)%>% endsWith("tl")]
#'   sp_data_sf <- purrr::reduce(col_names,
#'                               .init = sp_data_sf,
#'                               ~ add_prevalence_col(sp_data_sf = .x,
#'                                                    col_name = .y,
#'                                                    param_tb_slimmed = param_tb_slimmed,
#'                                                    par_name_var = par_name_var,
#'                                                    it_nbr = it_nbr))
#'   
#' 
#'   # t0_prev_pref <- t0_prev[1] %>% stringr::str_sub(end=-5)
#'   # tx_prev_pref <- tx_prev[1] %>% stringr::str_sub(end=-5)
#'   sp_data_sf <- ready.sim::total_t0_tx_cols(sp_data_sf = sp_data_sf, stat = "prev")
#'   # t0_totals <-  sp_data_sf %>%
#'   #   dplyr::select(t0_prev) %>%
#'   #   sf::st_set_geometry(NULL) %>%
#'   #   dplyr::mutate(!!rlang::sym(paste0(t0_prev_pref,"all")) := Reduce(`+`,.)) %>%
#'   #   dplyr::select(!!rlang::sym(paste0(t0_prev_pref,"all")))
#'   # tx_totals <-  sp_data_sf %>%
#'   #   dplyr::select(tx_prev) %>%
#'   #   sf::st_set_geometry(NULL) %>%
#'   #   dplyr::mutate(!!rlang::sym(paste0(tx_prev_pref,"all")) := Reduce(`+`,.)) %>%
#'   #   dplyr::select(!!rlang::sym(paste0(tx_prev_pref,"all")))
#'   # sp_data_sf <-   dplyr::bind_cols(sp_data_sf,t0_totals,tx_totals)
#'   # t0_prev <- c(t0_prev,paste0(t0_prev_pref,"all"))
#'   # tx_prev <- c(tx_prev,paste0(tx_prev_pref,"all"))
#'   t0_prev <- names(sp_data_sf)[names(sp_data_sf) %>% startsWith("t0_prev")]
#'   tx_prev <- names(sp_data_sf)[names(sp_data_sf) %>% startsWith("tx_prev")]
#'   t0_ind <- which(names(sp_data_sf) %in% t0_prev)
#'   tx_ind <- which(names(sp_data_sf) %in% tx_prev)
#'   index_vec <- 1:(length(t0_prev))
#'   purrr::reduce(index_vec,
#'                 ~ calc_change_in_prevalence(sp_data_sf = .x,
#'                                             col_ind = .y,
#'                                             t0_prev = t0_prev,
#'                                             tx_prev = tx_prev),
#'                 .init = sp_data_sf)
#' }
#' 
#' #' @title add_prevalence_col
#' #' @description FUNCTION_DESCRIPTION
#' #' @param sp_data_sf PARAM_DESCRIPTION
#' #' @param col_name PARAM_DESCRIPTION
#' #' @param param_tb_slimmed PARAM_DESCRIPTION
#' #' @param par_name_var PARAM_DESCRIPTION, Default: 'parameter_name'
#' #' @return OUTPUT_DESCRIPTION
#' #' @details DETAILS
#' #' @examples
#' #' \dontrun{
#' #' if(interactive()){
#' #'  #EXAMPLE1
#' #'  }
#' #' }
#' #' @seealso
#' #'  \code{\link[stringr]{str_sub}}
#' #'  \code{\link[dplyr]{pull}},\code{\link[dplyr]{mutate}}
#' #'  \code{\link[rlang]{sym}}
#' #'  \code{\link[ready.data]{data_get}}
#' #' @rdname add_prevalence_col
#' #' @importFrom stringr str_sub
#' #' @importFrom dplyr pull mutate
#' #' @importFrom rlang sym
#' #' @importFrom ready.data data_get
#' add_prevalence_col <- function(sp_data_sf,
#'                                col_name,
#'                                param_tb_slimmed,
#'                                par_name_var = "parameter_name",
#'                                it_nbr){
#'   age_sex_pair <- col_name %>% stringr::str_sub(start = -4)
#'   prev_pair <- param_tb_slimmed %>%
#'     dplyr::pull(!!par_name_var)
#'   prev_pair <- prev_pair[endsWith(prev_pair,age_sex_pair)]
#' 
#'   sp_data_sf %>%
#'     dplyr::mutate(!!rlang::sym(paste0(col_name %>% stringr::str_sub(end=2),
#'                                       "_",
#'                                       prev_pair)) := !!rlang::sym(col_name) * ready.data::data_get(data_lookup_tb = param_tb_slimmed,
#'                                                                                                    lookup_variable = !!par_name_var,
#'                                                                                                    lookup_reference = prev_pair,
#'                                                                                                    target_variable = paste0("v_it_", it_nbr),
#'                                                                                                    evaluate = FALSE))
#' }
#' @title calc_change_in_prevalence
#' @description FUNCTION_DESCRIPTION
#' @param sp_data_sf PARAM_DESCRIPTION
#' @param col_ind PARAM_DESCRIPTION
#' @param t0_prev PARAM_DESCRIPTION
#' @param tx_prev PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[stringr]{str_sub}}
#' @rdname calc_change_in_prevalence
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom stringr str_sub
calc_change_in_prevalence <- function(sp_data_sf,
                                      col_ind,
                                      t0_prev,
                                      tx_prev){
  sp_data_sf %>%
    dplyr::mutate(!!rlang::sym(paste0("delta",
                                      t0_prev[col_ind] %>% stringr::str_sub(start = 3))) :=
                    !!rlang::sym(tx_prev[col_ind]) - !!rlang::sym(t0_prev[col_ind]))
}



#' make_prev_summ_tb
#' FUNCTION_DESCRIPTION
#' @param prev_summary PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{mutate}}
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[tibble]{tibble}}
#' @rdname make_prev_summ_tb
#' @export
#' @importFrom dplyr summarise_at vars contains funs mutate
#' @importFrom stringr str_sub
#' @importFrom tibble tibble
make_prev_summ_tb <- function(prev_summary){
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
    dplyr::mutate(Persons = Females + Males)
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
