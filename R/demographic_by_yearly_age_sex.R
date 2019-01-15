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
                                          gen_projections = TRUE,
                                          drop_projs = FALSE){
  first_age_band <- c(floor(age0/intervals)*intervals,
                      ceiling(age0/intervals)*intervals-1)
  last_age_band <- c(floor(age1/intervals)*intervals,
                     ceiling(age1/intervals)*intervals-1)
  n_age_bands <- (last_age_band[1]-first_age_band[1])/intervals+1
  included_age_bands_num_all <- purrr::map(1:n_age_bands, ~ c(intervals*(.-1)+first_age_band[1],
                                                              intervals*(.-1)+first_age_band[2]))
  included_age_bands_num <- purrr::map(1:n_age_bands, ~ c(intervals*(.-1)+first_age_band[1],
                                                          intervals*(.-1)+first_age_band[2]))
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
                     ~ c(.[1],age1))
  included_time_intervals <- names(profiled_sf)[startsWith(names(profiled_sf),"y20")] %>%
    stringr::str_sub(2,5) %>%
    unique() %>%
    as.integer() %>%
    sort()
  years <- years %>% sort()
  t0_year <- years[1]
  t1_year <- years[length(years)]
  if(sum(t0_year>=included_time_intervals)==0||sum(t1_year<=included_time_intervals)==0)
    stop("c(t0,t1) outside bounds of available growth data. Try changing t0 and/or t1 values or enter an alternative profiled_sf")
  included_time_intervals <- included_time_intervals[max(which(included_time_intervals <= t0_year)):min(which(included_time_intervals >= t1_year))]
  included_time_intervals_start_end <- purrr::map(1:(length(included_time_intervals)-1),
                                                  ~ c(included_time_intervals[.],
                                                      included_time_intervals[.+1]))
  included_cols_pairs <- purrr::map(included_time_intervals_start_end,
                                    ~ list(t0 = included_cols[startsWith(included_cols,paste0("y",
                                                                                              as.character(.[1])))],
                                           t1 = included_cols[startsWith(included_cols,paste0("y",
                                                                                              as.character(.[2])))]))
  profiled_sf <- purrr::reduce(purrr::prepend(1:(length(included_cols_pairs)),
                                              list(a=profiled_sf)),
                               .f = function(x,y,...) x %>% col_pair_growth_rate(list_element = y,
                                                                                 included_cols_pairs = included_cols_pairs,
                                                                                 included_time_intervals_start_end = included_time_intervals_start_end ))
  if(gen_projections){
    profiled_sf <- gen_age_sex_projs_from_rate(profiled_sf,
                                               years,
                                               included_time_intervals,
                                               included_cols_pairs,
                                               included_age_bands_num,
                                               age0,
                                               age1,
                                               intervals)
  }
  if(drop_projs){
    profiled_sf <- profiled_sf %>%
      dplyr::select(-dplyr::starts_with("y20")) %>%
      dplyr::select(-dplyr::starts_with("growth."))
  }
  return(profiled_sf)
}

gen_age_sex_projs_from_rate <- function(profiled_sf,
                                        years,
                                        included_time_intervals,
                                        included_cols_pairs,
                                        included_age_bands_num,
                                        age0,
                                        age1,
                                        intervals){
  extra_years <- setdiff(years,
                         included_time_intervals)

  if(length(extra_years)>0){
    profiled_sf <- purrr::reduce(purrr::prepend(1:(extra_years %>%
                                                     length()),
                                                list(a=profiled_sf)),
                                 .f = function(x,y) x %>% add_extra_year_pop_totals(year = extra_years[y],
                                                                                    included_time_intervals = included_time_intervals,
                                                                                    included_cols_pairs = included_cols_pairs))
  }
  age_bands_vect <- included_age_bands_num %>% purrr::flatten_dbl()
  age_bands_ref <- purrr::map_dbl(age0:age1,
                                  ~ ceiling(max(which(age_bands_vect <= .)/2))
  )
  profiled_sf <- purrr::reduce(purrr::prepend(years,
                                              list(a=profiled_sf)),
                               .f = function(x,y) x %>% by_age_sex_for_a_year (age0 = age0,
                                                                               age1 = age1,
                                                                               year = y,
                                                                               age_bands_ref = age_bands_ref,
                                                                               intervals = intervals,
                                                                               included_age_bands_num_all = included_age_bands_num_all))
  return(profiled_sf)
}

#' @describeIn demographic_by_yearly_age_sex Calculates the compound annual growth rate between two periods.
#' @param t0_pop A numeric vector of population counts in the baseline year.
#' @param t1_pop A numeric vector of population counts in the follow-up year.
#' @param n_periods Number of years between baseline and follow-up.

demographic_compound_growth_rate <- function(t0_pop,
                                             t1_pop,
                                             n_periods){
  acgr <- (t1_pop/t0_pop)^(1/n_periods)-1
  return(acgr)
}

#' @describeIn demographic_by_yearly_age_sex Adds columns with the compound annual growth rate between two periods to inputted sf.
#' @param list_element Numeric, index value for look-up.
#' @param included_cols_pairs A list ....
#' @param included_time_intervals_start_end A list ....


col_pair_growth_rate <- function(profiled_sf,
                                 list_element,
                                 included_cols_pairs,
                                 included_time_intervals_start_end){
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
                                                                                                                                           purrr::pluck(list_element) %>% diff())))


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
