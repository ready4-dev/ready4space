#' @title Calculate age/sex populations for each year / age of interest.
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param years Numeric vector of the years for which age / sex counts should be estimated.
#' @param age0 Numeric, start of included age range.
#' @param age1 Numeric, end of included age range.
#' @param intervals Numeric, the years included in each age group in the source sf, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  demographic_by_yearly_age_sex(profiled_sf = spatial_vic_pop_growth_lga(vic_pop_growth_by_age_lga_t0 = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb, lookup_reference = "vic_pop_growth_by_age_lga_2016_tb"),
#'  vic_pop_growth_by_age_lga_t1 = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb, lookup_reference = "vic_pop_growth_by_age_lga_2031_tb"),
#'  t0 ="2016",
#'  t1 ="2031"),
#'  years = c(2016,2019, 2031, 2025),
#'  age0 = 4,
#'  age1 = 18)
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{flatten}},\code{\link[purrr]{modify}},\code{\link[purrr]{reduce}},\code{\link[purrr]{prepend}}
#'  \code{\link[stringr]{str_subset}},\code{\link[stringr]{str_sub}}
#' @rdname demographic_by_yearly_age_sex
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map flatten_chr modify_at reduce prepend flatten_dbl map_dbl pluck
#' @importFrom stringr str_subset str_sub str_replace
#' @importFrom dplyr mutate pull
#' @importFrom rlang sym



demographic_by_yearly_age_sex <- function(profiled_sf,
                                          years,
                                          age0,
                                          age1,
                                          intervals = 5,
                                          acgr = TRUE,
                                          age_by_year = FALSE,
                                          drop_bands = TRUE,
                                          param_tb,
                                          it_nbr){
  fn_pars <- gen_demog_fun_par_vals(profiled_sf,
                                    years,
                                    age0,
                                    age1,
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
                                            t0_year = fn_pars$t0_year,
                                            included_time_intervals = fn_pars$included_time_intervals,
                                            included_cols_pairs = fn_pars$included_cols_pairs,
                                            included_age_bands_num = fn_pars$included_age_bands_num,
                                            included_age_bands_num_all = fn_pars$included_age_bands_num_all,
                                            age0 = age0,
                                            age1 = age1,
                                            intervals = intervals)
  }
  if(drop_bands){
    profiled_sf <- profiled_sf %>%
      dplyr::select(-dplyr::starts_with("y20"))
  }
  return(profiled_sf)
}

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
                     dplyr::funs(stringr::str_sub(.,start = 4)))
}

gen_demog_fun_par_vals <- function(profiled_sf,
                                   years,
                                   age0,
                                   age1,
                                   intervals,
                                   acgr,
                                   age_by_year){
  first_age_band <- c(floor(age0 / intervals) * intervals,
                      ceiling(age0 / intervals) * intervals - 1)
  last_age_band <- c(floor(age1 / intervals) * intervals,
                     ceiling(age1 / intervals) * intervals - 1)
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
                                                                        ~ c(age0,.[2])) %>%
    purrr::modify_at(length(included_age_bands_num),
                     ~ c(.[1],
                         age1))
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




gen_age_sex_estimates_t0 <- function(profiled_sf,
                                     t0_year,
                                     included_time_intervals,
                                     included_cols_pairs,
                                     included_age_bands_num,
                                     included_age_bands_num_all,
                                     age0,
                                     age1,
                                     intervals){
  age_bands_vect <- included_age_bands_num %>% purrr::flatten_dbl()
  age_bands_ref <- purrr::map_dbl(age0:age1,
                                  ~ ceiling(max(which(age_bands_vect <= .)/2)))
  profiled_sf <- by_age_sex_for_a_year(profiled_sf = profiled_sf,
                                       age0 = age0,
                                       age1 = age1,
                                       year =  t0_year,
                                       age_bands_ref = age_bands_ref,
                                       intervals = intervals,
                                       included_age_bands_num_all = included_age_bands_num_all)
  return(profiled_sf)
}


#' @describeIn demographic_by_yearly_age_sex Calculates the compound annual growth rate between two periods.
#' @param t0_pop A numeric vector of population counts in the baseline year.
#' @param t1_pop A numeric vector of population counts in the follow-up year.
#' @param n_periods Number of years between baseline and follow-up.

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
  ape <- ready.data::data_get(data_lookup_tb = test_par_val_master,
                       lookup_reference = age_sex_lookup,
                       lookup_variable = "param_name",
                       target_variable = paste0("v_it_",it_nbr),
                       evaluate = FALSE)
  pe_sign <- ready.data::data_get(data_lookup_tb = param_tb,
                                     lookup_reference = "pop_pe_sign",
                                     lookup_variable = "param_name",
                                     target_variable = paste0("v_it_",it_nbr),
                                     evaluate = FALSE)
  pe <- ape * pe_sign
  growth_pc <- (t1_pop/t0_pop -1)*100
  adj_growth_pc <- growth_pc + pe
  adj_t1_pop = (adj_growth_pc / 100 +1) * t0_pop
  return(adj_t1_pop)
}

#' @describeIn demographic_by_yearly_age_sex Adds columns with the compound annual growth rate between two periods to inputted sf.
#' @param list_element Numeric, index value for look-up.
#' @param included_cols_pairs A list ....
#' @param included_time_intervals_start_end A list ....


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
                                .f = function(x,y) x %>% dplyr::mutate(!!(paste0("acgr_",
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
#' @describeIn demographic_by_yearly_age_sex Calculates a projected future population using the annual compound growth rate.
#' @param cgr Numeric, cumulative annual growth rate.
#' @param base_year_pop Numeric, population count in base year.
#' @param nyears Number of years between baseline and follow-up.

future_pop_from_comp_g_r <- function(cgr,
                                     base_year_pop,
                                     nyears){
  future_pop <- base_year_pop*(1+cgr)^nyears
  return(future_pop)
}


#' @describeIn demographic_by_yearly_age_sex Calculates grouped age / sex counts for additional years.
#' @param year Numeric, ....
#' @param included_cols_pairs A list, ....
#' @param included_time_intervals A list, ....

add_extra_year_pop_totals <- function(profiled_sf,
                                      year,
                                      included_time_intervals,
                                      included_cols_pairs){
  index_ref <- min(which(included_time_intervals<year))
  base_year <- included_time_intervals[index_ref]
  base_string <- included_cols_pairs %>% purrr::pluck(index_ref) %>% purrr::pluck(1)
  new_cols <- stringr::str_replace(base_string, as.character(base_year),as.character(year))
  acgr_cols <- paste0("acgr_",base_string)
  profiled_sf <-  purrr::reduce(purrr::prepend(1:(base_string %>%
                                                   length()),
                                               list(a=profiled_sf)),
                                .f = function(x,y,...) x %>% dplyr::mutate(!!new_cols[y] := future_pop_from_comp_g_r(cgr= x %>%
                                                                                                                       dplyr::pull(acgr_cols[y]),
                                                                                                                     base_year_pop = x %>%
                                                                                                                       dplyr::pull(base_string[y]),
                                                                                                                     nyears = year-base_year)))
  return(profiled_sf)
}

#' @describeIn demographic_by_yearly_age_sex Calculates age populations for each sex.
#' @param age Numeric, ...
#' @param year Numeric, ....
#' @param included_age_bands_num_all A list ....

by_sex_for_an_age <- function(profiled_sf,
                              age,
                              age0,
                              age1,
                              year,
                              age_bands_ref,
                              intervals,
                              included_age_bands_num_all){
  profiled_sf <- purrr::reduce(purrr::prepend(c("Females","Males"),
                                              list(a=profiled_sf)),
                               .f = function(x,y) x %>% dplyr::mutate(!!paste0("y_",
                                                                               year,
                                                                               "_",
                                                                               y,
                                                                               "_",
                                                                               age) := !!rlang::sym(paste0("y",
                                                                                                           year,
                                                                                                           ".",
                                                                                                           y,
                                                                                                           ".",
                                                                                                           included_age_bands_num_all  %>%
                                                                                                             purrr::pluck(age_bands_ref[which(age0:age1 == age)]) %>%
                                                                                                             purrr::pluck(1),
                                                                                                           ".",
                                                                                                           included_age_bands_num_all  %>%
                                                                                                             purrr::pluck(age_bands_ref[which(age0:age1 == age)]) %>%
                                                                                                             purrr::pluck(2))) / intervals))
  return(profiled_sf)
}
#' @describeIn demographic_by_yearly_age_sex Calculates age and sex populations for a specific year.
#' @param year Numeric, ....
#' @param included_age_bands_num_all A list ....

by_age_sex_for_a_year <- function(profiled_sf,
                                  age0,
                                  age1,
                                  year,
                                  age_bands_ref,
                                  intervals,
                                  included_age_bands_num_all){
  profiled_sf <- purrr::reduce(purrr::prepend(age0:age1,
                                              list(a=profiled_sf)),
                               .f = function(x,y) x %>% by_sex_for_an_age(age = y,
                                                                          age0 = age0,
                                                                          age1 = age1,
                                                                          year = year,
                                                                          age_bands_ref = age_bands_ref,
                                                                          intervals = intervals,
                                                                          included_age_bands_num_all = included_age_bands_num_all))
}


# DEPR_gen_age_sex_projs_from_rate_DEPR <- function(profiled_sf,
#                                         years,
#                                         included_time_intervals,
#                                         included_cols_pairs,
#                                         included_age_bands_num,
#                                         included_age_bands_num_all,
#                                         age0,
#                                         age1,
#                                         intervals){
#   extra_years <- setdiff(years,
#                          included_time_intervals)
#
#   if(length(extra_years)>0){
#     profiled_sf <- purrr::reduce(purrr::prepend(1:(extra_years %>%
#                                                      length()),
#                                                 list(a=profiled_sf)),
#                                  .f = function(x,y) x %>% add_extra_year_pop_totals(year = extra_years[y],
#                                                                                     included_time_intervals = included_time_intervals,
#                                                                                     included_cols_pairs = included_cols_pairs))
#   }
#   age_bands_vect <- included_age_bands_num %>% purrr::flatten_dbl()
#   age_bands_ref <- purrr::map_dbl(age0:age1,
#                                   ~ ceiling(max(which(age_bands_vect <= .)/2)))
#   profiled_sf <- purrr::reduce(purrr::prepend(years,
#                                               list(a=profiled_sf)),
#                                .f = function(x,y) x %>% by_age_sex_for_a_year (age0 = age0,
#                                                                                age1 = age1,
#                                                                                year = y,
#                                                                                age_bands_ref = age_bands_ref,
#                                                                                intervals = intervals,
#                                                                                included_age_bands_num_all = included_age_bands_num_all))
#   return(profiled_sf)
# }
