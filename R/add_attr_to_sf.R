#' @title recur_add_attr_to_sf
#' @description
#' Merges a SF file with population data relating to areas described in that file.
#' @details  Makes data transformations (variable name, changes from strings to factors) necessary
#' to ensure that merged objects are compatible.
#' @param input_data PARAM_DESCRIPTION
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
#'  \code{\link[ready.s4]{lookup_tb}},\code{\link[ready.s4]{sp_data_pack_lup}},\code{\link[ready.s4]{country}}
#'  \code{\link[dplyr]{filter}}
#'  \code{\link[ready.utils]{data_get}}
#'  \code{\link[stringr]{str_detect}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{prepend}},\code{\link[purrr]{reduce}}
#'  \code{\link[stats]{setNames}}
#' @rdname recur_add_attr_to_sf
#' @export
#' @importFrom ready.s4 lookup_tb sp_data_pack_lup country
#' @importFrom dplyr filter
#' @importFrom ready.utils data_get
#' @importFrom stringr str_detect
#' @importFrom rlang sym
#' @importFrom purrr map prepend reduce
#' @importFrom stats setNames
recur_add_attr_to_sf <- function(input_data,
                                 sub_div_unit = NULL,
                                 area_unit,
                                 boundary_year,
                                 attribute_data
                                 ){
 lookup_tb_r4 <- ready.s4::lookup_tb(input_data$profiled_area_input)
  data_lookup_tb <- ready.s4::sp_data_pack_lup(lookup_tb_r4) %>%
    dplyr::filter(area_type == area_unit)
  b_yr <- boundary_year ## Temporary: NEED TO RENAME boundary_year argument within function
  ##### ADD BOUNDARY FILE TO DATA IMPORT
  boundary_file <- parse(text = ready.utils::data_get(data_lookup_tb = data_lookup_tb %>%
                                          dplyr::filter(boundary_year == b_yr),
                                       lookup_reference = "Boundary",
                                       lookup_variable = "main_feature",
                                       target_variable = "source_reference",
                                       evaluate = FALSE)) %>% eval()
  country <- ready.s4::country(input_data$profiled_area_input)
  if(!is.null(sub_div_unit)){ ### REWRITE AS ABSTRACT WITH LOOK-UP
    if(country=="Australia")
      sub_div_unit_var_name <- names(boundary_file)[names(boundary_file) %>% stringr::str_detect("STE_NAME")]
    # paste0("STE_NAME",stringr::str_sub(boundary_year,3,4))
    boundary_file <- boundary_file %>%
      dplyr::filter(!!rlang::sym(sub_div_unit_var_name) == sub_div_unit)
  }
  boundary_file_as_list <- list(sf = boundary_file)

  attribute_data_list <-purrr::map(attribute_data,
                                   ~ .x) %>%
    stats::setNames(attribute_data)
  reduce_list <- purrr::prepend(attribute_data_list,
                                boundary_file_as_list)
  purrr::reduce(reduce_list,
                ~ add_attr_list_to_sf(.x,
                                      .y,
                                      area_unit = area_unit,
                                      boundary_year = boundary_year,
                                      data_lookup_tb = data_lookup_tb,
                                      sub_div_unit = sub_div_unit))
}
##
#' @title add_attr_list_to_sf
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param y PARAM_DESCRIPTION
#' @param area_unit PARAM_DESCRIPTION
#' @param boundary_year PARAM_DESCRIPTION
#' @param data_lookup_tb PARAM_DESCRIPTION
#' @param sub_div_unit PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready.utils]{data_get}}
#' @rdname add_attr_list_to_sf
#' @export
#' @importFrom ready.utils data_get
add_attr_list_to_sf <- function(x,
                                y,
                                area_unit,
                                boundary_year,
                                data_lookup_tb,
                                sub_div_unit){
  add_attr_to_sf(area_unit = area_unit,
                 area_sf = x,
                 attr_data_tb = eval(parse(text = ready.utils::data_get(data_lookup_tb = data_lookup_tb,
                                                                       lookup_reference = y,
                                                                       lookup_variable = "name",
                                                                       target_variable = "transformation",
                                                                       evaluate = FALSE))),
                 attr_data_desc = ready.utils::data_get(data_lookup_tb = data_lookup_tb,
                                                       lookup_reference = y,
                                                       lookup_variable = "name",
                                                       target_variable = "main_feature",
                                                       evaluate = FALSE),
                 attr_data_year = ready.utils::data_get(data_lookup_tb = data_lookup_tb,
                                                       lookup_reference = y,
                                                       lookup_variable = "name",
                                                       target_variable = "year",
                                                       evaluate = FALSE),
                 boundary_year = boundary_year,
                 sub_div_unit = sub_div_unit )
}
## EVERYTHING BELOW NEEDS TO BE INTEGRATED WITH australia.r4ext
#' @title add_attr_to_sf
#' @description FUNCTION_DESCRIPTION
#' @param area_unit PARAM_DESCRIPTION
#' @param area_sf PARAM_DESCRIPTION
#' @param attr_data_tb PARAM_DESCRIPTION
#' @param attr_data_desc PARAM_DESCRIPTION
#' @param attr_data_year PARAM_DESCRIPTION
#' @param boundary_year PARAM_DESCRIPTION
#' @param sub_div_unit PARAM_DESCRIPTION
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
add_attr_to_sf <- function(area_unit,
                           area_sf,
                           attr_data_tb,
                           attr_data_desc,
                           attr_data_year,
                           boundary_year,
                           sub_div_unit
                           ){
  if(attr_data_desc == "Population projections"){
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb)
  }
  if(stringr::str_detect(attr_data_desc, "ERP by age and sex")){
      merged_units <- dplyr::inner_join(area_sf,
                                        attr_data_tb) %>%
        sf::st_as_sf()
  }
  if(attr_data_desc == "ERP"){
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

