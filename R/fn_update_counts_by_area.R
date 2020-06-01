#' @title update_pop_count_by_areas
#' @description Adjust population counts by fraction of spatial unit included in a profiled area.
#' @param profiled_sf PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param age_sex_var_name PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname update_pop_count_by_areas
#' @export

update_pop_count_by_areas <-function(profiled_sf,
                                     group_by_var,
                                     age_sex_var_name,
                                     data_year,
                                     age_sex_pop_resolution,
                                     tot_pop_resolution,
                                     popl_var_prefix = ""){
  profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                        sp_unit = age_sex_pop_resolution,
                                        data_year = data_year,
                                        concept = "age_sex",
                                        popl_var_prefix = popl_var_prefix)
  if(popl_var_prefix == "")
    profiled_sf <- sum_pop_by_multiple_groups_sf(profiled_sf = profiled_sf,
                                                 group_by_var = group_by_var,
                                                 age_sex_var_name = age_sex_var_name,
                                                 data_year = data_year,
                                                 age_sex_pop_resolution = age_sex_pop_resolution,
                                                 tot_pop_resolution = tot_pop_resolution,
                                                 popl_var_prefix = popl_var_prefix)

  return(profiled_sf)
}

#' @title update_pop_by_inc_area
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param sp_unit PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param concept PARAM_DESCRIPTION
#' @param age_sex_var_name PARAM_DESCRIPTION, Default: NULL
#' @param age_sex_pop_resolution PARAM_DESCRIPTION, Default: NULL
#' @param tot_pop_col PARAM_DESCRIPTION, Default: NULL
#' @param popl_var_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{select_all}}
#'  \code{\link[rlang]{sym}}
#'  \code{\link[sf]{geos_measures}}
#'  \code{\link[units]{set_units}}
#' @rdname update_pop_by_inc_area
#' @export
#' @importFrom dplyr mutate mutate_at vars starts_with funs rename_at
#' @importFrom rlang sym
#' @importFrom sf st_area
#' @importFrom units set_units
update_pop_by_inc_area <- function(profiled_sf,
                                   sp_unit,
                                   data_year,
                                   concept,
                                   age_sex_var_name = NULL,
                                   age_sex_pop_resolution = NULL,
                                   tot_pop_col = NULL,
                                   popl_var_prefix){
  nse_objs_ls <- gen_objs_for_nse_upd_pop(sp_unit = sp_unit,
                                          concept = concept,
                                          tot_pop_col = tot_pop_col,
                                          grouping_1 = age_sex_pop_resolution,
                                          data_year = data_year,
                                          popl_var_prefix = popl_var_prefix)
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$area_inc_unit) := sf::st_area(.) %>%
                    units::set_units(km^2))
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$prop_inc_unit) := as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit)))
  if(!is.null(tot_pop_col)){
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(!!rlang::sym(nse_objs_ls$popl_inc_unit) := !!rlang::sym(nse_objs_ls$popl_whl_unit) * !!rlang::sym(nse_objs_ls$prop_inc_unit))
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          nse_objs_ls = nse_objs_ls,
                                          grp_var_name = age_sex_var_name)
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(pop_prop_multiplier_tot_pop = !!rlang::sym(nse_objs_ls$popl_inc_unit) / !!rlang::sym(nse_objs_ls$grouping_1_concept_tot))  %>%
      dplyr::mutate(pop_prop_multiplier_tot_pop = ifelse(is.nan(pop_prop_multiplier_tot_pop),
                                                         0,
                                                         pop_prop_multiplier_tot_pop))
  }
  profiled_sf <- profiled_sf %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_1),
                                 dplyr::starts_with(nse_objs_ls$popl_whl_starts_with_2)),
                     dplyr::funs(!!rlang::sym(nse_objs_ls$popl_inc_unit) := .*!!rlang::sym(nse_objs_ls$popl_multiplier))) %>%
    suffix_to_prefix(suffix = nse_objs_ls$popl_inc_unit) %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                     dplyr::funs(gsub(nse_objs_ls$inc_str_to_delete,
                                      "",
                                      .)))
  return(profiled_sf)
}

#' @title gen_objs_for_nse_upd_pop
#' @param sp_unit PARAM_DESCRIPTION
#' @param concept PARAM_DESCRIPTION
#' @param tot_pop_col PARAM_DESCRIPTION, Default: NULL
#' @param grouping_1 PARAM_DESCRIPTION, Default: NULL
#' @param data_year PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname gen_objs_for_nse_upd_pop
#' @export

gen_objs_for_nse_upd_pop <- function(sp_unit,
                                     concept,
                                     tot_pop_col = NULL,
                                     grouping_1 = NULL,
                                     data_year,
                                     popl_var_prefix){
  if(concept == "age_sex"){
    popl_multiplier <- paste0("inc_",sp_unit,"_prop")
    whl_pop_str_1 <- paste0("whl_",sp_unit,"_",popl_var_prefix,"y",data_year,".Females.")
    whl_pop_str_2 <- paste0("whl_",sp_unit,"_",popl_var_prefix,"y",data_year,".Males.")
    inc_str_to_delete <- paste0("whl_",sp_unit,"_")
    grouping_1_age_sex_pop_str <- NA_character_
  }
  if(concept == "tot_pop"){
    popl_multiplier <- "pop_prop_multiplier_tot_pop"
    grouping_1_age_sex_pop_str <- paste0("grp_by_",grouping_1,"_inc_age_sex_")
    whl_pop_str_1 <- paste0(grouping_1_age_sex_pop_str,"y",data_year,".Females.")
    whl_pop_str_2 <- paste0(grouping_1_age_sex_pop_str,"y",data_year,".Males.")
    inc_str_to_delete <- grouping_1_age_sex_pop_str
    grouping_1_age_sex_pop_str <- paste0("grp_by_",grouping_1,"_inc_age_sex_")
  }
  list(area_whl_unit = paste0("whl_",sp_unit,"_area"),
       area_inc_unit = paste0("inc_",sp_unit,"_area"),
       prop_inc_unit = paste0("inc_",sp_unit,"_prop"),
       popl_inc_unit = paste0("inc_",sp_unit,"_popl"),
       popl_whl_unit = paste0("whl_",sp_unit,"_",tot_pop_col),
       popl_multiplier = popl_multiplier,
       popl_whl_starts_with_1 = ifelse(is.null(whl_pop_str_1),
                                       NA_character_,
                                       whl_pop_str_1),
       popl_whl_starts_with_2 = ifelse(is.null(whl_pop_str_2),
                                       NA_character_,
                                       whl_pop_str_2),
       grouping_1_concept_tot = ifelse(is.null(grouping_1),
                                       NA_character_,
                                       paste0("grp_by_",
                                              grouping_1,
                                              "_inc_",
                                              concept)),
       grouping_1_age_sex_pop = grouping_1_age_sex_pop_str,
       inc_str_to_delete = inc_str_to_delete)
}

#' @title sum_updated_pop_by_grp
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param grp_var_name PARAM_DESCRIPTION
#' @param nse_objs_ls PARAM_DESCRIPTION
#' @param suff_to_pref PARAM_DESCRIPTION, Default: FALSE
#' @param top_level PARAM_DESCRIPTION, Default: FALSE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[sf]{st_geometry}}
#'  \code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{bind}},\code{\link[dplyr]{join}}
#'  \code{\link[rlang]{sym}}
#' @rdname sum_updated_pop_by_grp
#' @export
#' @importFrom sf st_set_geometry
#' @importFrom dplyr group_by summarise_at vars starts_with funs ungroup rename_at bind_cols inner_join
#' @importFrom rlang sym
sum_updated_pop_by_grp <- function(profiled_sf,
                                   grp_var_name,
                                   nse_objs_ls,
                                   suff_to_pref = FALSE,
                                   top_level = FALSE){
  group_totals <- profiled_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(!!rlang::sym(grp_var_name)) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                        dplyr::funs(!!rlang::sym(nse_objs_ls$grouping_1_concept_tot) := sum(.)))
  if(suff_to_pref)
    group_totals <- group_totals %>%
      suffix_to_prefix(suffix = nse_objs_ls$grouping_1_concept_tot)
  group_totals <- group_totals %>%
    dplyr::ungroup() %>%
    dplyr::rename_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$grouping_1_concept_tot)),
                     dplyr::funs(gsub(paste0(nse_objs_ls$popl_inc_unit,"_"),"",.)))
  if(top_level){
    dplyr::bind_cols(profiled_sf,
                     group_totals[rep(row.names(group_totals), nrow(profiled_sf)), ])
  }else{
    profiled_sf %>%
      dplyr::inner_join(.,group_totals)
  }
}

#' @title sum_pop_by_multiple_groups_sf
#' @description FUNCTION_DESCRIPTION
#' @param profiled_sf PARAM_DESCRIPTION
#' @param group_by_var PARAM_DESCRIPTION
#' @param age_sex_var_name PARAM_DESCRIPTION
#' @param data_year PARAM_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION
#' @param popl_var_prefix PARAM_DESCRIPTION, Default: ''
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sum_pop_by_multiple_groups_sf
#' @export

sum_pop_by_multiple_groups_sf <- function(profiled_sf,
                                          group_by_var,
                                          age_sex_var_name,
                                          data_year,
                                          age_sex_pop_resolution,
                                          tot_pop_resolution,
                                          popl_var_prefix = ""){
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                        grp_var_name = age_sex_var_name,
                                        nse_objs_ls = gen_objs_for_nse_upd_pop(sp_unit = age_sex_pop_resolution,
                                                                               concept = "age_sex",
                                                                               grouping_1 = age_sex_pop_resolution,
                                                                               data_year = data_year,
                                                                               popl_var_prefix = popl_var_prefix),
                                        suff_to_pref = TRUE)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                          sp_unit = tot_pop_resolution,
                                          data_year = data_year,
                                          concept = "tot_pop",
                                          tot_pop_col = paste0("year_",
                                                               data_year,
                                                               "pr"),
                                          age_sex_var_name = age_sex_var_name,
                                          age_sex_pop_resolution = age_sex_pop_resolution)
  }
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = age_sex_var_name,
                                          nse_objs_ls = gen_objs_for_nse_upd_pop(sp_unit = tot_pop_resolution,
                                                                                 concept = "tot_pop",
                                                                                 tot_pop_col = paste0("year_",
                                                                                                      data_year,
                                                                                                      "pr"),
                                                                                 grouping_1 = age_sex_pop_resolution,
                                                                                 data_year = data_year),
                                          suff_to_pref = TRUE)
  }
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                        grp_var_name = group_by_var,
                                        nse_objs_ls = gen_objs_for_nse_upd_pop(age_sex_pop_resolution,
                                                                               concept = "age_sex",
                                                                               grouping_1 = age_sex_pop_resolution,
                                                                               data_year = data_year,
                                                                               popl_var_prefix = popl_var_prefix),
                                        suff_to_pref = TRUE,
                                        top_level = TRUE)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = group_by_var,
                                          nse_objs_ls = gen_objs_for_nse_upd_pop(sp_unit = tot_pop_resolution,
                                                                                 concept = "tot_pop",
                                                                                 tot_pop_col = paste0("year_",
                                                                                                      data_year,
                                                                                                      "pr"),
                                                                                 grouping_1 = age_sex_pop_resolution,
                                                                                 data_year = data_year),
                                          suff_to_pref = TRUE,
                                          top_level = TRUE)
  }
}

#' @title suffix_to_prefix
#' @description FUNCTION_DESCRIPTION
#' @param data_tb PARAM_DESCRIPTION
#' @param suffix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{funs}}
#' @rdname suffix_to_prefix
#' @export
#' @importFrom dplyr rename_at vars ends_with funs
suffix_to_prefix <- function(data_tb,
                             suffix){ ##### MOVE THIS TO READY4FUN (AND UPDATE ALL REFERENCES TO THIS FUNCTION)
  data_tb %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix)),
                     dplyr::funs(paste0(suffix,
                                        "_",
                                        gsub(paste0("_",
                                                    suffix),"",.))))
}

#' @title get_popl_var_prefix
#' @description FUNCTION_DESCRIPTION
#' @param age_sex_pop_resolution PARAM_DESCRIPTION
#' @param tot_pop_resolution PARAM_DESCRIPTION, Default: NULL
#' @param data_year PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname get_popl_var_prefix
#' @export

get_popl_var_prefix <- function(age_sex_pop_resolution,
                                tot_pop_resolution = NULL,
                                data_year){
  if(!is.null(tot_pop_resolution)){
    nse_names_ls <- gen_objs_for_nse_upd_pop(sp_unit = tot_pop_resolution,
                                             concept = "tot_pop",
                                             tot_pop_col = paste0("year_",
                                                                  data_year,
                                                                  "pr"),
                                             grouping_1 = age_sex_pop_resolution,
                                             data_year = data_year)
  }else{
    nse_names_ls <- gen_objs_for_nse_upd_pop(sp_unit = age_sex_pop_resolution,

                                             concept = "age_sex",
                                             grouping_1 = age_sex_pop_resolution,
                                             data_year = data_year)
  }
  paste0(nse_names_ls$popl_inc_unit,"_")
}

