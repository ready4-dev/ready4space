#' @title
#' Adjust population counts by fraction of spatial unit included in a profiled area.
#'
#' @description
#' This function:
#'  -
#'  -
#'
#' @details Need to review if removal of linestrings is appropriate.
#'
#' @param tot_pop_sf A simple features object comprised of SA1s
#'
#' @param profiled_sf A simple features object comprised of SA2s.
#'
#' @param group_by_var A string specifying "SA1" or "SA2" as the resolution of the
#' returned object,
#'
#' @return
#' A simple features object.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{join}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{reexports}},\code{\link[dplyr]{select_all}},\code{\link[dplyr]{vars}},\code{\link[dplyr]{funs}},\code{\link[dplyr]{summarise_all}}
#'  \code{\link[sf]{st_geometry}},\code{\link[sf]{geos_measures}}
#' @rdname update_pop_count_by_areas
#' @export
#' @importFrom dplyr inner_join group_by summarise ungroup rename mutate filter select contains rename_at vars funs mutate_at summarise_at starts_with
#' @importFrom sf st_set_geometry st_area

update_pop_count_by_areas <-function(profiled_sf,
                                     group_by_var,
                                     age_sex_var_name,
                                     data_year,
                                     age_sex_pop_resolution,
                                     tot_pop_resolution){
  profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                        sp_unit = age_sex_pop_resolution,
                                        data_year = data_year)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                          sp_unit = tot_pop_resolution,
                                          data_year = data_year,
                                          tot_pop_col = paste0("year_",
                                                               data_year,
                                                               "pr"),
                                          age_sex_var_name = age_sex_var_name)
  }
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                        grp_var_name = age_sex_var_name,
                                        nse_objs_ls = gen_objs_for_nse_upd_pop(age_sex_pop_resolution))

  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = age_sex_var_name,
                                          nse_objs_ls = gen_objs_for_nse_upd_pop(tot_pop_resolution))
  }
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                         grp_var_name = group_by_var,
                         nse_objs_ls = gen_objs_for_nse_upd_pop(age_sex_pop_resolution),
                         top_level = TRUE)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = group_by_var,
                                          nse_objs_ls = gen_objs_for_nse_upd_pop(tot_pop_resolution),
                                          top_level = TRUE)
  }
  return(profiled_sf)
}
suffix_to_prefix <- function(data_tb,
                             suffix){
  data_tb %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(suffix)),
                     dplyr::funs(paste0(suffix,
                                        "_",
                                        gsub(paste0("_",
                                                    suffix),"",.))))
}
update_pop_by_inc_area <- function(profiled_sf,
                                   sp_unit,
                                   data_year,
                                   age_sex_var_name = NULL,
                                   age_sex_pop_resolution = NULL,
                                   tot_pop_col = NULL){
  nse_objs_ls <- gen_objs_for_nse_upd_pop(sp_unit = sp_unit)
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$area_inc_unit) := sf::st_area(.) %>% units::set_units(km^2))
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$prop_inc_unit) := as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit))) %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$prop_inc_unit) := ifelse(is.nan(!!rlang::sym(nse_objs_ls$prop_inc_unit)),
                                                                    0,
                                                                    !!rlang::sym(nse_objs_ls$prop_inc_unit)))
  if(is.null(tot_pop_col)){
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(pop_prop_multiplier = !!rlang::sym(nse_objs_ls$prop_inc_unit))
  }else{
    popl_inc_unit <- paste0(nse_objs_ls$popl_inc_unit,"_",tot_pop_col)
    grpd_by_asu_tot_pop <- paste0("grpd_by_",age_sex_pop_resolution,"_",popl_inc_unit)
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(!!rlang::sym(popl_inc_unit) := !!rlang::sym(tot_pop_col) * !!rlang::sym(nse_objs_ls$prop_inc_unit))
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          nse_objs_ls = nse_objs_ls,
                                          grp_var_name = age_sex_var_name)
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(pop_prop_multiplier = !!rlang::sym(popl_inc_unit) / !!rlang::sym(paste0("grpd_",age_sex_var_name,
                                                                                            "_",
                                                                                            "grpd_",age_sex_var_name))) ## UPDATE
  }
  profiled_sf <- profiled_sf %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(paste0("y",data_year,".Females.")),
                                 dplyr::starts_with(paste0("y",data_year,".Males."))),
                     dplyr::funs(!!rlang::sym(nse_objs_ls$popl_inc_unit) :=.*pop_prop_multiplier)) %>%
    suffix_to_prefix(suffix = nse_objs_ls$popl_inc_unit)
  return(profiled_sf)
}
gen_objs_for_nse_upd_pop <- function(sp_unit){
  list(area_whl_unit = paste0("area_whl_",sp_unit),
       area_inc_unit = paste0("area_inc_",sp_unit),
       prop_inc_unit = paste0("prop_inc_",sp_unit),
       popl_inc_unit = paste0("popl_inc_",sp_unit),
       popl_whl_unit = paste0("popl_whl_",sp_unit))
}
sum_updated_pop_by_grp <- function(profiled_sf,
                                   grp_var_name,
                                   nse_objs_ls,
                                   top_level = FALSE){
  group_prefix <- paste0("grpd_",grp_var_name)
  group_totals <- profiled_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(!!rlang::sym(grp_var_name)) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                        dplyr::funs(!!rlang::sym(group_prefix) := sum(.))) %>% # REMOVED NA.RM ARG
    suffix_to_prefix(suffix = group_prefix) %>%
    dplyr::ungroup()
  if(top_level){
    dplyr::bind_cols(profiled_sf,
          group_totals[rep(row.names(group_totals), nrow(profiled_sf)), ])
  }else{
    profiled_sf %>%
      dplyr::inner_join(.,group_totals)
  }
}
#s1 missing
