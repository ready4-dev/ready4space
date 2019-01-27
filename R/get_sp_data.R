#' @title get_spatial_data_list
#' @description Create a list of SF objects constructed from specified data types at highest or requested level of resolution.
#' @param at_highest_res PARAM_DESCRIPTION
#' @param at_time PARAM_DESCRIPTION
#' @param to_time PARAM_DESCRIPTION, Default: NULL
#' @param at_specified_res PARAM_DESCRIPTION, Default: NULL
#' @param country PARAM_DESCRIPTION, Default: 'Australia'
#' @param state PARAM_DESCRIPTION, Default: NULL
#' @param require_year_match PARAM_DESCRIPTION, Default: TRUE
#' @param excl_diff_bound_yr PARAM_DESCRIPTION, Default: TRUE
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
#'  \code{\link[purrr]{map}},\code{\link[purrr]{map2}},\code{\link[purrr]{prepend}}
#'  \code{\link[stats]{setNames}}
#' @rdname get_spatial_data_list
#' @export
#' @importFrom stringr str_sub
#' @importFrom purrr map map2 map_lgl prepend
#' @importFrom stats setNames

get_spatial_data_list <- function(at_highest_res,
                                  at_time,
                                  to_time = NULL,
                                  at_specified_res = NULL,
                                  country = "Australia",
                                  state = NULL,
                                  require_year_match = TRUE,
                                  excl_diff_bound_yr = TRUE){

  attributes_to_import <- get_spatial_data_names(at_highest_res = at_highest_res,
                                                 at_time = at_time,
                                                 to_time = to_time,
                                                 at_specified_res = at_specified_res,
                                                 country = country,
                                                 state = state,
                                                 require_year_match = require_year_match,
                                                 excl_diff_bound_yr = excl_diff_bound_yr)

  boundary_res <- stringr::str_sub(attributes_to_import,5,7) %>% unique() %>% toupper()
  data_names_list <- purrr::map(boundary_res,
                                ~ attributes_to_import[stringr::str_sub(attributes_to_import,5,7) == tolower(.x )]) %>%
    stats::setNames(boundary_res)
  data_sf_list <- purrr::map2(boundary_res,
                              data_names_list,
                              ~ recur_add_attr_to_sf(country = country,
                                                     state = state,
                                                     area_unit = .x,
                                                     boundary_year = at_time,
                                                     attribute_data = .y)) %>%
    stats::setNames(boundary_res)
  index_ppr <- purrr::map_lgl(data_names_list,
                              ~ check_if_ppr(.x,
                                             data_lookup_tb = aus_spatial_lookup_tb)) %>%
    which() + 1
  data_sf_list <- purrr::prepend(data_sf_list,
                                 list(index_ppr=index_ppr))
  return(data_sf_list)
}

get_spatial_data_names <- function(at_highest_res,
                                   at_time,
                                   to_time = NULL,
                                   at_specified_res = NULL,
                                   country = "Australia",
                                   state = NULL,
                                   require_year_match = TRUE,
                                   excl_diff_bound_yr = TRUE){ #### NEED TO WORK ON SECOND HALF
  if(excl_diff_bound_yr){
    spatial_lookup_tb <- aus_spatial_lookup_tb %>%
      dplyr::filter(is.na(additional_detail) | additional_detail != " for 2016 boundaries")
  }else
    spatial_lookup_tb <- aus_spatial_lookup_tb
  year_vec <- as.character(as.numeric(at_time):as.numeric(to_time))
  lookup_tb_list <- purrr::map(at_highest_res,
                               ~ spatial_lookup_tb %>%
                                 dplyr::filter(main_feature == .x) %>%
                                 dplyr::filter(year %in% year_vec[if(.x=="Population projections") 1:length(year_vec) else 1]))
  data_res_vec <- purrr::map_chr(lookup_tb_list,
                                 ~ .x %>%
                                   dplyr::pull(area_type) %>%
                                   unique() %>%
                                   get_highest_res(year = at_time))
  data_unavail_for_year <-  is.na(data_res_vec)
  if(require_year_match & sum(data_unavail_for_year) > 0)
    stop("Data not available for specified year for all data requested")
  matched_year_vec <- at_highest_res[!data_unavail_for_year]
  matched_yr_lookup_tb_list <- lookup_tb_list[!data_unavail_for_year]
  matched_yr_data_res_vec <- data_res_vec[!data_unavail_for_year]
  non_matched_year_vec <- at_highest_res[is.na(data_res_vec)]
  matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
                                           matched_yr_data_res_vec,
                                           ~ .x %>%
                                             dplyr::filter(area_type == .y))
  if(!is.null(state)){
    region_lookup <- purrr::map_chr(state,
                                    ~ ready.data::data_get(data_lookup_tb = aus_state_short_tb,
                                                           lookup_reference = .,
                                                           lookup_variable = "state_territory",
                                                           target_variable = "short_name",
                                                           evaluate = FALSE))
    matched_yr_lookup_tb_list <- purrr::map2(matched_yr_lookup_tb_list,
                                             region_lookup,
                                             ~  .x %>% dplyr::filter(region %in% .y))
  }
  names_of_data_vec <- purrr::map(matched_yr_lookup_tb_list,
                                  ~ .x %>%
                                    dplyr::pull(name)) %>%
    purrr::flatten_chr()

  if(!identical(non_matched_year_vec,character(0))){
    closest_years <- get_closest_year(incl_main_ft_vec = non_matched_year_vec,
                                      target_year = at_time)
    extra_names <- purrr::map2_chr(non_matched_year_vec,closest_years,
                                   ~     ready.data::data_get(data_lookup_tb = spatial_lookup_tb %>%
                                                                dplyr::filter(year == .y),
                                                              lookup_reference = .x,
                                                              lookup_variable = "main_feature",
                                                              target_variable = "name",
                                                              evaluate = FALSE))
  }
  names_of_data_vec <- c(names_of_data_vec,extra_names)
  extra_names <- purrr::map_chr(at_specified_res,
                                ~ spatial_lookup_tb %>%
                                  dplyr::filter(year %in% year_vec) %>%
                                  dplyr::filter(area_type == .x[2]) %>%
                                  ready.data::data_get(lookup_reference = .x[1],
                                                       lookup_variable = "main_feature",
                                                       target_variable = "name",
                                                       evaluate = FALSE)) %>%
    unname()
  c(names_of_data_vec,extra_names)
}

get_closest_year <- function(data_lookup_tb = aus_spatial_lookup_tb,
                             incl_main_ft_vec,
                             target_year,
                             target_area = NULL,
                             find_closest = "abs"){
  if(!is.null(target_area)){
    data_lookup_tb <- data_lookup_tb %>%
      dplyr::filter(area_type == target_area)
  }
  avail_years <- purrr::map(incl_main_ft_vec,
                            ~ data_lookup_tb %>%
                              dplyr::filter(main_feature == .x) %>%
                              dplyr::pull(year) %>%
                              as.numeric())
  if(find_closest == "abs"){
    closest_year <- purrr::map(avail_years,
                               ~ .x[which(abs(.x - as.numeric(target_year)) == min(abs(.x - as.numeric(target_year))))])
  }
  if(find_closest == "previous"){
    closest_year <- purrr::map(avail_years,
                               ~ .x[which(as.numeric(target_year) - .x == min(max(as.numeric(target_year) - .x,0)))])
  }

  if(find_closest == "next"){
    closest_year <- purrr::map(avail_years,
                               ~ .x[which(.x - as.numeric(target_year) == min(max(.x - as.numeric(target_year),0)))])
  }
  return(closest_year)
}

get_resolution_hierarchy <- function(data_year,
                                     resolution_tb = aus_data_resolution_tb,
                                     whole_area = TRUE){
  resolution_hierarchy <- resolution_tb  %>%
    dplyr::filter(boundary_year == data_year)
  if(whole_area){
    resolution_hierarchy <- resolution_hierarchy %>%
      dplyr::filter(complete==TRUE)
  }
  resolution_hierarchy %>%
    dplyr::arrange(dplyr::desc(area_count)) %>%
    dplyr::pull(area_type)
}

get_highest_res <- function(options_vec,
                            year){
  if(!is.na(options_vec[1])){
    res_hierarchy <- get_resolution_hierarchy(as.numeric(at_time))
    res_hierarchy[min(which(res_hierarchy %in% options_vec))]
  }else
    NA
}

##

check_if_ppr <- function(data_name_item,
                         data_lookup_tb){
  purrr::map_chr(data_name_item,
                 ~ ready.data::data_get(data_lookup_tb = data_lookup_tb,
                                        lookup_reference = .x,
                                        lookup_variable = "name",
                                        target_variable = "main_feature",
                                        evaluate = FALSE)) %>%
    stringr::str_detect("Population projections") %>%
    sum() %>%
    magrittr::is_greater_than(0)
}
