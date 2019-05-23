#' @title Calculate age/sex populations for each year / age of interest.
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param years Numeric vector of the years for which age / sex counts should be estimated.
#' @param age_lower Numeric, start of included age range.
#' @param age_upper Numeric, end of included age range.
#' @param param_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
#' @param acgr PARAM_DESCRIPTION, Default: TRUE
#' @param age_by_year PARAM_DESCRIPTION, Default: FALSE
#' @param drop_bands PARAM_DESCRIPTION, Default: TRUE
#' @param intervals Numeric, the years included in each age group in the source sf, Default: 5
#' @param month PARAM_DESCRIPTION, Default: '07'
#' @param day PARAM_DESCRIPTION, Default: '01'
#' @param included_prefix PARAM_DESCRIPTION, Default: ''
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
#' @rdname gen_demog_features
#' @export
#' @importFrom dplyr select starts_with
gen_demog_features <- function(profiled_sf,
                               years,
                               age_lower,
                               age_upper,
                               param_tb,
                               it_nbr,
                               acgr = TRUE,
                               age_by_year = FALSE,
                               drop_bands = TRUE,
                               intervals = 5,
                               month = "07",
                               day = "01",
                               included_prefix = ""){
  fn_pars <- gen_demog_fun_par_vals(profiled_sf,
                                    years,
                                    age_lower,
                                    age_upper,
                                    intervals,
                                    acgr,
                                    age_by_year)
  if(acgr){
    profiled_sf <- gen_demog_acgr(profiled_sf,
                                  fn_pars,
                                  param_tb,
                                  it_nbr)
  }
  if(age_by_year){
    profiled_sf <- gen_age_sex_estimates_t0(profiled_sf = profiled_sf,
                                            t0_date = paste0(fn_pars$t0_year,month,day),
                                            included_age_bands_num = fn_pars$included_age_bands_num,
                                            included_age_bands_num_all = fn_pars$included_age_bands_num_all,
                                            age_lower = age_lower,
                                            age_upper = age_upper,
                                            intervals = intervals,
                                            included_prefix = included_prefix)
  }
  if(drop_bands){
    profiled_sf <- profiled_sf %>%
      dplyr::select(-dplyr::starts_with("y20")) %>%
      dplyr::select(-dplyr::starts_with(paste0(included_prefix,"y20")))
  }
  return(profiled_sf)
}

#' gen_demog_acgr
#' FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param fn_pars PARAM_DESCRIPTION
#' @param param_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{reduce}},\code{\link[purrr]{prepend}}
#'  \code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{select}}
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{case}}
#' @rdname gen_demog_acgr
#' @export
#' @importFrom purrr reduce prepend
#' @importFrom dplyr rename_at vars starts_with funs select
#' @importFrom stringr str_sub str_to_lower
gen_demog_acgr <- function(profiled_sf,
                           fn_pars,
                           param_tb,
                           it_nbr){
  profiled_sf <- purrr::reduce(purrr::prepend(1:(length(fn_pars$included_cols_pairs)),
                                              list(a=profiled_sf)),
                               .f = function(x,y,...) x %>% col_pair_growth_rate(list_element = y,
                                                                                 included_cols_pairs = fn_pars$included_cols_pairs,
                                                                                 included_time_intervals_start_end = fn_pars$included_time_intervals_start_end,
                                                                                 param_tb = param_tb,
                                                                                 it_nbr = it_nbr))
  profiled_sf <- profiled_sf %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with(paste0("y",fn_pars$t0_year))),
                     dplyr::funs(paste0("t0_",.))) %>%
    dplyr::select(-dplyr::starts_with("y20")) %>%
    dplyr::select(-dplyr::starts_with("growth.")) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with(paste0("t0_y",fn_pars$t0_year))),
                     dplyr::funs(stringr::str_sub(.,start = 4))) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with("acgr_y")),
                     dplyr::funs(paste0(stringr::str_sub(.,1,10),
                                        "_",
                                        stringr::str_sub(.,12,12) %>% stringr::str_to_lower(),
                                        "_",
                                        stringr::str_sub(.,-5,-4),
                                        "_",
                                        stringr::str_sub(.,-2,-1))))
}

#' gen_demog_fun_par_vals
#' FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param years PARAM_DESCRIPTION
#' @param age_lower PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param intervals PARAM_DESCRIPTION
#' @param acgr PARAM_DESCRIPTION
#' @param age_by_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{flatten}},\code{\link[purrr]{modify}}
#'  \code{\link[stringr]{str_subset}},\code{\link[stringr]{str_sub}}
#' @rdname gen_demog_fun_par_vals
#' @export
#' @importFrom purrr map flatten_chr modify_at
#' @importFrom stringr str_subset str_sub
gen_demog_fun_par_vals <- function(profiled_sf,
                                   years,
                                   age_lower,
                                   age_upper,
                                   intervals,
                                   acgr,
                                   age_by_year){
  first_age_band <- c(floor(age_lower / intervals) * intervals,
                      ifelse(age_lower %% intervals == 0, age_lower/intervals + 1, ceiling(age_lower/intervals)) * intervals - 1)
  last_age_band <- c(floor(age_upper / intervals) * intervals,
                     ifelse(age_upper %% intervals == 0, age_upper/intervals + 1, ceiling(age_upper/intervals)) * intervals - 1)
  n_age_bands <- (last_age_band[1] - first_age_band[1]) / intervals + 1
  included_age_bands_num_all <- purrr::map(1:n_age_bands, ~ c(intervals * (. - 1) + first_age_band[1],
                                                              intervals * (. - 1) + first_age_band[2]))
  included_age_bands_num <- purrr::map(1:n_age_bands, ~ c(intervals * (. - 1) + first_age_band[1],
                                                          intervals * (. - 1) + first_age_band[2]))
  included_age_bands_chr <- purrr::map(included_age_bands_num ,
                                       ~ paste0(.[1],
                                                ".",
                                                .[2])) %>%
    purrr::flatten_chr()
  included_cols <- purrr::map(included_age_bands_chr,
                              ~ stringr::str_subset(names(profiled_sf), .)) %>%
    purrr::flatten_chr() %>%
    sort()
  included_age_bands_num <- included_age_bands_num %>% purrr::modify_at(1,
                                                                        ~ c(age_lower,.[2])) %>%
    purrr::modify_at(length(included_age_bands_num),
                     ~ c(.[1],
                         age_upper))
  if(!acgr & age_by_year){
    included_time_intervals <- names(profiled_sf)[startsWith(names(profiled_sf),"acgr_y20")] %>%
      stringr::str_sub(7, 10)
  }else{
    included_time_intervals <- names(profiled_sf)[startsWith(names(profiled_sf),"y20")] %>%
      stringr::str_sub(2, 5)
  }
  included_time_intervals <- included_time_intervals %>%
    unique() %>%
    as.integer() %>%
    sort()
  years <- years %>% sort()
  t0_year <- years[1]
  t1_year <- years[length(years)]
  if(!acgr & age_by_year & sum(t1_year <= included_time_intervals) == 0)
    included_time_intervals <- c(included_time_intervals,t1_year)
  if(sum(t0_year >= included_time_intervals) == 0||sum(t1_year <= included_time_intervals) == 0)
    stop("Start/end year outside bounds of available growth data. Try changing years values or enter an alternative profiled_sf")
  included_time_intervals <- included_time_intervals[max(which(included_time_intervals <= t0_year)):min(which(included_time_intervals >= t1_year))]
  included_time_intervals_start_end <- purrr::map(1:(length(included_time_intervals)-1),
                                                  ~ c(included_time_intervals[.],
                                                      included_time_intervals[.+1]))
  included_cols_pairs <- purrr::map(included_time_intervals_start_end,
                                    ~ list(t0 = included_cols[startsWith(included_cols,paste0("y",
                                                                                              as.character(.[1])))],
                                           t1 = included_cols[startsWith(included_cols,paste0("y",
                                                                                              as.character(.[2])))]))
  return(list(included_cols_pairs=included_cols_pairs,
              included_age_bands_num = included_age_bands_num,
              included_age_bands_num_all = included_age_bands_num_all,
              included_time_intervals = included_time_intervals,
              included_time_intervals_start_end = included_time_intervals_start_end,
              t0_year = t0_year,
              t1_year = t1_year))
}

#' gen_age_sex_estimates_t0
#' FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param t0_date PARAM_DESCRIPTION
#' @param included_age_bands_num PARAM_DESCRIPTION
#' @param included_age_bands_num_all PARAM_DESCRIPTION
#' @param age_lower PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param intervals PARAM_DESCRIPTION
#' @param included_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{flatten}},\code{\link[purrr]{map}}
#' @rdname gen_age_sex_estimates_t0
#' @export
#' @importFrom purrr flatten_dbl map_dbl
gen_age_sex_estimates_t0 <- function(profiled_sf,
                                     t0_date,
                                     included_age_bands_num,
                                     included_age_bands_num_all,
                                     age_lower,
                                     age_upper,
                                     intervals,
                                     included_prefix){
  age_bands_vect <- included_age_bands_num %>% purrr::flatten_dbl()
  age_bands_ref <- purrr::map_dbl(age_lower:age_upper,
                                  ~ ceiling(max(which(age_bands_vect <= .)/2)))
  profiled_sf <- by_age_sex_for_a_year(profiled_sf = profiled_sf,
                                       age_lower = age_lower,
                                       age_upper = age_upper,
                                       t0_date =  t0_date,
                                       age_bands_ref = age_bands_ref,
                                       intervals = intervals,
                                       included_age_bands_num_all = included_age_bands_num_all,
                                       included_prefix = included_prefix)
  return(profiled_sf)
}

#' by_sex_for_an_age
#' FUNCTION_DESCRIPTION
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param age PARAM_DESCRIPTION
#' @param age_lower PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param t0_date PARAM_DESCRIPTION
#' @param age_bands_ref PARAM_DESCRIPTION
#' @param intervals PARAM_DESCRIPTION
#' @param included_age_bands_num_all PARAM_DESCRIPTION
#' @param included_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{case}}
#'  \code{\link[purrr]{reduce}},\code{\link[purrr]{prepend}},\code{\link[purrr]{pluck}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#' @rdname by_sex_for_an_age
#' @export
#' @importFrom stringr str_sub str_to_lower
#' @importFrom purrr reduce prepend pluck
#' @importFrom dplyr mutate
#' @importFrom rlang sym
by_sex_for_an_age <- function(profiled_sf,
                              age,
                              age_lower,
                              age_upper,
                              t0_date,
                              age_bands_ref,
                              intervals,
                              included_age_bands_num_all,
                              included_prefix){
  year <- t0_date %>% stringr::str_sub(1,4)
  profiled_sf <- purrr::reduce(purrr::prepend(c("Females","Males"),
                                              list(a=profiled_sf)),
                               .f = function(x,y) x %>% dplyr::mutate(!!paste0("t0_",
                                                                               t0_date,
                                                                               "_",
                                                                               stringr::str_sub(y,1,1) %>%
                                                                                 stringr::str_to_lower(),
                                                                               "_",
                                                                               age) := !!rlang::sym(paste0(included_prefix,
                                                                                                           "y",
                                                                                                           year,
                                                                                                           ".",
                                                                                                           y,
                                                                                                           ".",
                                                                                                           included_age_bands_num_all  %>%
                                                                                                             purrr::pluck(age_bands_ref[which(age_lower:age_upper == age)]) %>%
                                                                                                             purrr::pluck(1),
                                                                                                           ".",
                                                                                                           included_age_bands_num_all  %>%
                                                                                                             purrr::pluck(age_bands_ref[which(age_lower:age_upper == age)]) %>%
                                                                                                             purrr::pluck(2))) / intervals))
  return(profiled_sf)
}
#' by_age_sex_for_a_year
#' FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param age_lower PARAM_DESCRIPTION
#' @param age_upper PARAM_DESCRIPTION
#' @param t0_date PARAM_DESCRIPTION
#' @param age_bands_ref PARAM_DESCRIPTION
#' @param intervals PARAM_DESCRIPTION
#' @param included_age_bands_num_all PARAM_DESCRIPTION
#' @param included_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{reduce}},\code{\link[purrr]{prepend}}
#' @rdname by_age_sex_for_a_year
#' @export
#' @importFrom purrr reduce prepend
by_age_sex_for_a_year <- function(profiled_sf,
                                  age_lower,
                                  age_upper,
                                  t0_date,
                                  age_bands_ref,
                                  intervals,
                                  included_age_bands_num_all,
                                  included_prefix){
  profiled_sf <- purrr::reduce(purrr::prepend(age_lower:age_upper,
                                              list(a=profiled_sf)),
                               .f = function(x,y) x %>% by_sex_for_an_age(age = y,
                                                                          age_lower = age_lower,
                                                                          age_upper = age_upper,
                                                                          t0_date = t0_date,
                                                                          age_bands_ref = age_bands_ref,
                                                                          intervals = intervals,
                                                                          included_age_bands_num_all = included_age_bands_num_all,
                                                                          included_prefix = included_prefix))
}

#' demographic_compound_growth_rate
#' FUNCTION_DESCRIPTION
#' @param t0_pop PARAM_DESCRIPTION
#' @param t1_pop PARAM_DESCRIPTION
#' @param n_periods PARAM_DESCRIPTION
#' @param age_sex_band PARAM_DESCRIPTION
#' @param param_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname demographic_compound_growth_rate
#' @export

demographic_compound_growth_rate <- function(t0_pop,
                                             t1_pop,
                                             n_periods,
                                             age_sex_band,
                                             param_tb,
                                             it_nbr){
  t1_pop <- adjust_pop_proj_for_pe(t0_pop,
                                   t1_pop,
                                   n_periods,
                                   age_sex_band,
                                   param_tb,
                                   it_nbr)
  acgr <- (t1_pop/t0_pop)^(1/n_periods)-1
  return(acgr)
}

#' adjust_pop_proj_for_pe
#' ADD DESCRIPTIVE INFO HERE
#' @param t0_pop PARAM_DESCRIPTION
#' @param t1_pop PARAM_DESCRIPTION
#' @param n_periods PARAM_DESCRIPTION
#' @param age_sex_band PARAM_DESCRIPTION
#' @param param_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_sub}}
#'  \code{\link[ready.utils]{data_get}}
#' @rdname adjust_pop_proj_for_pe
#' @export
#' @importFrom stringr str_sub
#' @importFrom ready.utils data_get
adjust_pop_proj_for_pe <- function(t0_pop,
                                   t1_pop,
                                   n_periods,
                                   age_sex_band,
                                   param_tb,
                                   it_nbr){
  if(n_periods < 10){
    yrs <- paste0("0",as.character(n_periods))
  }else
    yrs <- as.character(n_periods)
  age_sex_lookup <- paste0("mape_",
                           yrs,
                           "_yr_",
                           stringr::str_sub(age_sex_band,1,1),
                           stringr::str_sub(age_sex_band,-6))
  ape <- ready.utils::data_get(data_lookup_tb = param_tb,
                       lookup_reference = age_sex_lookup,
                       lookup_variable = "parameter_name",
                       target_variable = paste0("v_it_",it_nbr),
                       evaluate = FALSE)
  pe_sign <- ready.utils::data_get(data_lookup_tb = param_tb,
                                     lookup_reference = "pop_pe_sign",
                                     lookup_variable = "parameter_name",
                                     target_variable = paste0("v_it_",it_nbr),
                                     evaluate = FALSE)
  pe <- ape * pe_sign
  growth_pc <- (t1_pop/t0_pop -1)*100
  adj_growth_pc <- growth_pc + pe
  adj_t1_pop = (adj_growth_pc / 100 +1) * t0_pop
  return(adj_t1_pop)
}

#' col_pair_growth_rate
#' ADD DESCRIPTIVE INFO HERE
#' @param profiled_sf PARAM_DESCRIPTION
#' @param list_element PARAM_DESCRIPTION
#' @param included_cols_pairs PARAM_DESCRIPTION
#' @param included_time_intervals_start_end PARAM_DESCRIPTION
#' @param param_tb PARAM_DESCRIPTION
#' @param it_nbr PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{reduce}},\code{\link[purrr]{prepend}},\code{\link[purrr]{pluck}}
#'  \code{\link[dplyr]{mutate}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_replace}},\code{\link[stringr]{case}}
#' @rdname col_pair_growth_rate
#' @export
#' @importFrom purrr reduce prepend pluck
#' @importFrom dplyr mutate
#' @importFrom rlang sym
#' @importFrom stringr str_sub str_replace_all str_to_lower
col_pair_growth_rate <- function(profiled_sf,
                                 list_element,
                                 included_cols_pairs,
                                 included_time_intervals_start_end,
                                 param_tb,
                                 it_nbr){
  profiled_sf  <- purrr::reduce(purrr::prepend(1:(included_cols_pairs %>%
                                                    purrr::pluck(list_element) %>%
                                                    purrr::pluck("t0") %>%
                                                    length()),
                                               list(a=profiled_sf)),
                                .f = function(x,y) x %>%
                                  dplyr::mutate(!!(paste0("acgr_",
                                                          included_cols_pairs %>%
                                                            purrr::pluck(list_element) %>%
                                                            purrr::pluck("t0") %>%
                                                            purrr::pluck(y))) := demographic_compound_growth_rate(t0_pop = !!rlang::sym(included_cols_pairs %>%
                                                                                                                                          purrr::pluck(list_element) %>%
                                                                                                                                          purrr::pluck("t0") %>%
                                                                                                                                          purrr::pluck(y)),
                                                                                                                  t1_pop = !!rlang::sym(included_cols_pairs %>%
                                                                                                                                          purrr::pluck(list_element) %>%
                                                                                                                                          purrr::pluck("t1") %>%
                                                                                                                                          purrr::pluck(y)),
                                                                                                                  n_periods = included_time_intervals_start_end %>%
                                                                                                                    purrr::pluck(list_element) %>% diff(),
                                                                                                                  age_sex_band = included_cols_pairs %>%
                                                                                                                    purrr::pluck(list_element) %>%
                                                                                                                    purrr::pluck("t1") %>%
                                                                                                                    purrr::pluck(y) %>%
                                                                                                                    stringr::str_sub(start = 7) %>%
                                                                                                                    stringr::str_replace_all("\\.","_") %>%
                                                                                                                    stringr::str_to_lower(),
                                                                                                                  param_tb = param_tb,
                                                                                                                  it_nbr = it_nbr)))


  return(profiled_sf)
}

#' future_pop_from_comp_g_r
#' FUNCTION_DESCRIPTION
#' @param cgr Numeric, cumulative annual growth rate.
#' @param base_year_pop Numeric, population count in base year.
#' @param nyears Number of years between baseline and follow-up.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname future_pop_from_comp_g_r
#' @export

future_pop_from_comp_g_r <- function(cgr,
                                     base_year_pop,
                                     nyears){
  future_pop <- base_year_pop*(1+cgr)^nyears
  return(future_pop)
}


#' gen_age_sex_estimates_tx
#' FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param ymwd_step PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{pull}}
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_replace}}
#'  \code{\link[purrr]{pluck}},\code{\link[purrr]{map}},\code{\link[purrr]{reduce}},\code{\link[purrr]{prepend}}
#'  \code{\link[lubridate]{ymd}},\code{\link[lubridate]{period}},\code{\link[lubridate]{interval}}
#' @rdname gen_age_sex_estimates_tx
#' @export
#' @importFrom dplyr mutate_at vars starts_with funs rename_at contains select mutate pull
#' @importFrom stringr str_sub str_replace str_replace_all
#' @importFrom purrr pluck map_chr reduce prepend
#' @importFrom lubridate ymd years weeks days interval
gen_age_sex_estimates_tx <- function(profiled_sf,
                                     ymwd_step){
  if(profiled_sf %>% names() %>% startsWith("t0_") %>% sum() ==0){
    stop("Inputted SF object does not have baseline year population counts by age. See help for gen_demog_features function for
         details about creating SF objects with the required features.")
  }
  if(profiled_sf %>% names() %>% startsWith("tx_") %>% sum() ==0){
    profiled_sf <- profiled_sf %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("t0_")),
                       dplyr::funs(tx = .*1)) %>%
      dplyr::rename_at(dplyr::vars(dplyr::contains("_tx")),
                       dplyr::funs(paste0("tx_",
                                          stringr::str_sub(.,4,-4))))
  }
  tx_cols <- profiled_sf %>%
    dplyr::select(dplyr::starts_with("tx_")) %>%
    names() %>%
    setdiff("geometry")
  t0_date <- tx_cols  %>%
    stringr::str_sub(4,11) %>%
    purrr::pluck(1) %>%
    lubridate::ymd()
  step_in_time <- lubridate::years(ymwd_step[1]) +
    months(ymwd_step[2]) +
    lubridate::weeks(ymwd_step[3]) +
    lubridate::days(ymwd_step[4])
  t1_date <- t0_date + step_in_time
  step_in_years <- lubridate::interval(t0_date,t1_date) / (lubridate::years(1))
  gr_cols <- profiled_sf %>%
    dplyr::select(dplyr::starts_with("acgr_y")) %>%
    names() %>%
    setdiff("geometry")
  gr_base_years <- stringr::str_sub(gr_cols,7,10) %>%
    as.numeric()
  gr_base_unique <- unique(gr_base_years)
  matched_gr_year <- max(gr_base_unique[gr_base_unique<=as.numeric(t0_date %>% stringr::str_sub(1,4))])
  gr_cols_matched_yr <- gr_cols[gr_base_years == matched_gr_year]
  gr_cols_age_low <- stringr::str_sub(gr_cols_matched_yr,-5,-4)
  gr_cols_age_high <- stringr::str_sub(gr_cols_matched_yr,-2,-1)
  gr_cols_sex <- stringr::str_sub(gr_cols_matched_yr,-7,-7)
  gr_cols_matched_categ <- purrr::map_chr(tx_cols,
                                          ~ gr_cols_matched_yr[(gr_cols_age_low <= stringr::str_sub(.x,-2,-1) %>% as.numeric()) +
                                            (stringr::str_sub(.x,-2,-1) %>% as.numeric() <= gr_cols_age_high)  +
                                            (gr_cols_sex==stringr::str_sub(.x,-4,-4)) ==3])
  purrr::reduce(purrr::prepend(1:(tx_cols %>%
                                    length()),
                               list(a=profiled_sf)),
                .f = function(x,y,...) x %>% dplyr::mutate(!!tx_cols[y] := future_pop_from_comp_g_r(cgr= x %>%
                                                                                                      dplyr::pull(gr_cols_matched_categ[y]),
                                                                                                    base_year_pop = x %>%
                                                                                                      dplyr::pull(tx_cols[y]),
                                                                                                    nyears = step_in_years))) %>%
    dplyr::rename_at(dplyr::vars(tx_cols),
                     dplyr::funs(stringr::str_replace(.,
                                                      t0_date %>% stringr::str_replace_all("-",""),
                                                      t1_date %>% stringr::str_replace_all("-",""))))
}




