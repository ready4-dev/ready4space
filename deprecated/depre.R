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
# DEPR_add_extra_year_pop_totals_DEPR <- function(profiled_sf,
#                                       year,
#                                       included_time_intervals,
#                                       included_cols_pairs){
#   index_ref <- min(which(included_time_intervals<year))
#   base_year <- included_time_intervals[index_ref]
#   base_string <- included_cols_pairs %>% purrr::pluck(index_ref) %>% purrr::pluck(1)
#   new_cols <- stringr::str_replace(base_string, as.character(base_year),as.character(year))
#   acgr_cols <- paste0("acgr_",base_string)
#   profiled_sf <-  purrr::reduce(purrr::prepend(1:(base_string %>%
#                                                    length()),
#                                                list(a=profiled_sf)),
#                                 .f = function(x,y,...) x %>% dplyr::mutate(!!new_cols[y] := future_pop_from_comp_g_r(cgr= x %>%
#                                                                                                                        dplyr::pull(acgr_cols[y]),
#                                                                                                                      base_year_pop = x %>%
#                                                                                                                        dplyr::pull(base_string[y]),
#                                                                                                                      nyears = year-base_year)))
#   return(profiled_sf)
# }
