#' @title recur_add_attr_to_sf
#' @description
#' Merges a SF file with population data relating to areas described in that file.
#' @details  Makes data transformations (variable name, changes from strings to factors) necessary
#' to ensure that merged objects are compatible.
#' @param input_ls PARAM_DESCRIPTION
#' @param sub_div_unit PARAM_DESCRIPTION, Default: NULL
#' @param area_unit PARAM_DESCRIPTION
#' @param boundary_year PARAM_DESCRIPTION
#' @param attribute_data PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[stringr]{str_detect}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{prepend}},\code{\link[purrr]{reduce}}
#'  \code{\link[stats]{setNames}}
#' @rdname recur_add_attr_to_sf
#' @export
#' @importFrom dplyr filter
#' @importFrom ready4utils data_get
#' @importFrom stringr str_detect
#' @importFrom rlang sym
#' @importFrom purrr map prepend reduce
#' @importFrom stats setNames
recur_add_attr_to_sf <- function(input_ls,
                                 sub_div_unit = NULL,
                                 area_unit,
                                 boundary_year,
                                 attribute_data
                                 ){
  lookup_tb_r4 <- lookup_tb(input_ls$pa_r4)
  data_lookup_tb <- sp_data_pack_lup(lookup_tb_r4)
  boundary_file <- parse(text = ready4utils::data_get(data_lookup_tb = data_lookup_tb %>%
                                                        dplyr::filter(area_type == area_unit) %>%
                                                        dplyr::filter(main_feature == "Boundary") %>%
                                          dplyr::filter(as.numeric(year_start) == max(as.numeric(year_start)[as.numeric(year_start) <= as.numeric(boundary_year)])), # boundary_year
                                       lookup_reference = "Boundary",
                                       lookup_variable = "main_feature",
                                       target_variable = "source_reference",
                                       evaluate = FALSE)) %>% eval()
  attribute_data_list <- purrr::map(attribute_data,
                                   ~ .x) %>%
    stats::setNames(attribute_data)
  purrr::map(attribute_data_list, ~ add_attr_list_to_sf(x = boundary_file,
                                                        y = .x,
                                                        lookup_tb_r4 = lookup_tb_r4)) %>%
    subset_sf_ls_by_common_vars() %>%
    purrr::reduce(~rbind(.x,.y))
}

#' @title subset_sf_ls_by_common_vars
#' @description FUNCTION_DESCRIPTION
#' @param sf_ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#'  \code{\link[dplyr]{select}}
#' @rdname subset_sf_ls_by_common_vars
#' @export
#' @importFrom purrr map
#' @importFrom dplyr select
subset_sf_ls_by_common_vars <- function(sf_ls){
common_vars_vec <- get_common_vars_sf_ls(sf_ls)
purrr::map(sf_ls, ~ .x %>% dplyr::select(common_vars_vec))

}

#' @title get_common_vars_sf_ls
#' @description FUNCTION_DESCRIPTION
#' @param sf_ls PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}}
#' @rdname get_common_vars_sf_ls
#' @export
#' @importFrom purrr map
get_common_vars_sf_ls <- function(sf_ls){
  vec_ls <- purrr::map(sf_ls, ~ names(.x))
  Reduce(intersect, vec_ls)
}
### MOVE THESE TO UTILITIES - NOT USED YET
get_common_yrs_sf_ls <- function(sf_ls){
  vec_ls <- purrr::map(list_of_sfs, ~ get_included_yrs_sf(.x))
  Reduce(intersect, vec_ls)
}
get_max_or_min_yr_of_sf <- function(sf,
                                    max = T){
  year_vec <- get_included_yrs_sf(sf)
  if(max)
     max(year_vec)
  else
    min(year_vec)
}
get_included_yrs_sf <- function(sf){
  sf %>%
    sf::`st_geometry<-`(NULL) %>%
    dplyr::select(dplyr::starts_with("y2")) %>%
    names() %>%
    stringr::str_sub(start = 2, end = 5) %>%
    unique() %>%
    as.numeric()
}
##
#' @title add_attr_list_to_sf
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param lookup_tb_r4 PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{data_get}}
#' @rdname add_attr_list_to_sf
#' @export
#' @importFrom ready4utils data_get
add_attr_list_to_sf <- function(x,
                                y,
                                lookup_tb_r4){
  attr_data_xx <- make_attr_data_xx(lookup_tb_r4 = lookup_tb_r4,
                                    lookup_ref = y,
                                    starter_sf = x)
  add_attr_to_sf(area_sf = x,
                 attr_data_tb = attr_data_xx,
                 attr_data_desc = ready4utils::data_get(data_lookup_tb = sp_data_pack_lup(lookup_tb_r4),
                                                       lookup_reference = y,
                                                       lookup_variable = "name",
                                                       target_variable = "main_feature",
                                                       evaluate = FALSE)
                 )
}

#' @title make_attr_data_xx
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tb_r4 PARAM_DESCRIPTION
#' @param lookup_ref PARAM_DESCRIPTION
#' @param starter_sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready4utils]{data_get}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}}
#' @rdname make_attr_data_xx
#' @export
#' @importFrom ready4utils data_get
#' @importFrom stats setNames
#' @importFrom dplyr filter pull
make_attr_data_xx <- function(lookup_tb_r4,
                              lookup_ref,
                              starter_sf){
  data_lookup_tb <- sp_data_pack_lup(lookup_tb_r4)
  attr_data_xx <- eval(parse(text = ready4utils::data_get(data_lookup_tb = data_lookup_tb,
                                                          lookup_reference = lookup_ref,
                                                          lookup_variable = "name",
                                                          target_variable = "source_reference", #transformation
                                                          evaluate = FALSE)))
  if(is.data.frame(attr_data_xx)){
    attr_data_xx <- list(attr_data_xx) %>%
      stats::setNames(ready4utils::data_get(data_lookup_tb = data_lookup_tb,
                                            lookup_reference = lookup_ref,
                                            lookup_variable = "name",
                                            target_variable = "year",
                                            evaluate = FALSE))
  }
    region_short_nm <- ready4utils::data_get(data_lookup_tb = data_lookup_tb,
                                             lookup_reference = lookup_ref,
                                             lookup_variable = "name",
                                             target_variable = "region",
                                             evaluate = FALSE)
    region_short_long_vec <- c(region_short_nm,
                               ready4utils::data_get(data_lookup_tb = sp_abbreviations_lup(lookup_tb_r4),
                                                     lookup_reference = region_short_nm,
                                                     lookup_variable = "short_name",
                                                     target_variable = "long_name",
                                                     evaluate = FALSE))
    area_names_var_str <- ready4utils::data_get(data_lookup_tb = data_lookup_tb,
                                                lookup_reference = lookup_ref,
                                                lookup_variable = "name",
                                                target_variable = "area_type",
                                                evaluate = FALSE) %>%
      ready4utils::data_get(data_lookup_tb = sp_starter_sf_lup(lookup_tb_r4),
                            lookup_reference = .,
                            lookup_variable = "area_type",
                            target_variable = "sf_main_sub_div",
                            evaluate = FALSE)
    area_names_var_str <- area_names_var_str[area_names_var_str %in% names(starter_sf)]
    boundary_year <- ready4utils::data_get(data_lookup_tb = data_lookup_tb,
                                           lookup_reference = lookup_ref,
                                           lookup_variable = "name",
                                           target_variable = "area_bound_yr",
                                           evaluate = F)
    area_names_var_str <- sp_uid_lup(lookup_tb_r4) %>%
      dplyr::filter(var_name %in% area_names_var_str) %>%
      dplyr::filter(as.numeric(year) == max(as.numeric(year)[as.numeric(year) <= as.numeric(boundary_year)])) %>%
      dplyr::pull(var_name)
    updateAttrDataXx(lookup_tb_r4,
                     attr_data_xx = attr_data_xx,
                     alt_names_sf = starter_sf,
                     area_names_var_str = area_names_var_str,
                     region_short_long_vec = region_short_long_vec,
                     lookup_ref = lookup_ref)
}
## EVERYTHING BELOW NEEDS TO BE INTEGRATED WITH australia.r4ext
#' @title add_attr_to_sf
#' @description FUNCTION_DESCRIPTION
#' @param area_sf PARAM_DESCRIPTION
#' @param attr_data_tb PARAM_DESCRIPTION
#' @param attr_data_desc PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{join}}
#'  \code{\link[stringr]{str_detect}}
#'  \code{\link[sf]{st_as_sf}}
#' @rdname add_attr_to_sf
#' @export
#' @importFrom dplyr inner_join
#' @importFrom stringr str_detect
#' @importFrom sf st_as_sf
add_attr_to_sf <- function(#area_unit,
                           area_sf,
                           attr_data_tb,
                           attr_data_desc#,
                           #attr_data_year,
                           #boundary_year,
                           #sub_div_unit
                           ){
  if(attr_data_desc == "PPR"){ # "Population projections"
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb)
  }
  if(stringr::str_detect(attr_data_desc, "ERP_TOT")){ #"ERP by age and sex"
      merged_units <- dplyr::inner_join(area_sf,
                                        attr_data_tb) %>%
        sf::st_as_sf()
  }
  if(attr_data_desc == "ERP_ASX"){ # "ERP"
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  ##
  ## DON'T DELETE THE FOLLOWING SECTION UNTIL HAVE IMPLEMENTED PROCESSING OF SEIFA IN australia.r4ext
  ##
  ##
  # if(attr_data_desc == "SEIFA"){
  #   t1_stub <- stringr::str_sub(boundary_year,start=3,end=4)
  #   attr_data_tb <- australia.r4ext::prepare_seifa_data(seifa_data = attr_data_tb,
  #                                              area_unit = area_unit,
  #                                              #attr_data_year = attr_data_year,
  #                                              t1_stub = t1_stub)
  #   if(area_unit == "LGA"){
  #     groupvar <- rlang::sym(paste0("LGA_CODE",t1_stub))
  #     unitname <- paste0("LGA_NAME",t1_stub)
  #   }
  #   if(area_unit=="SA2"){
  #     groupvar <- rlang::sym(paste0("SA2_MAIN",t1_stub))
  #     unitname <- paste0("SA2_NAME",t1_stub)
  #   }
  #   merged_units <- dplyr::left_join(area_sf,
  #                                    attr_data_tb)
  #   merged_units <- dplyr::inner_join(merged_units,
  #                                     merged_units %>%
  #                                       dplyr::group_by(!!groupvar) %>%
  #                                       dplyr::summarise(resident.pop.all.parts=sum(Usual.Res.Pop)) %>%
  #                                       dplyr::ungroup() %>%
  #                                       sf::st_set_geometry(NULL))
  #   merged_units <- dplyr::inner_join(merged_units,
  #                                     australia.r4ext::summarise_seifa(seifa_sf = merged_units,
  #                                                     groupvar = groupvar,
  #                                                     unitname = unitname) %>%
  #                                       sf::st_set_geometry(NULL)) %>%
  #     dplyr::select(-c("Usual.Res.Pop",
  #                       "Score",
  #                       "Rank.Australia",
  #                       "Decile.Australia",
  #                       "Percentile.Australia",
  #                       "State",
  #                       "Rank.ST",
  #                       "Decile.ST",
  #                       "Percentile.ST",
  #                       "Minimum score for SA1s in area",
  #                       "Maximum score for SA1s in area",
  #                       "% Usual Resident Population without an SA1 level score",
  #                       "resident.pop.all.parts"))
  # }##
  return(merged_units)
}

