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
#' @rdname spatial_adjust_population_by_included_fraction
#' @export
#' @importFrom dplyr inner_join group_by summarise ungroup rename mutate filter select contains rename_at vars funs mutate_at summarise_at starts_with
#' @importFrom sf st_set_geometry st_area

spatial_adjust_population_by_included_fraction<-function(profiled_sf,
                                                         group_by_var,
                                                         age_sex_var_name,
                                                         data_year,
                                                         age_sex_pop_resolution,
                                                         tot_pop_resolution){
  ## OPTION 1: IF BASING POP COUNTS ON PROPORTION OF AGE/SEX RESOLUTION UNIT INCLUDED
  profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                        sp_unit = age_sex_pop_resolution)
  if(!is.null(tot_pop_resolution)){
    profiled_sf <- update_pop_by_inc_area(profiled_sf = profiled_sf,
                                          sp_unit = tot_pop_resolution)
  }
  #age_sex_nse_objs_ls <- gen_objs_for_nse_upd_pop(age_sex_pop_resolution)
  profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                         grp_var_name = age_sex_var_name,
                         nse_objs_ls = gen_objs_for_nse_upd_pop(age_sex_pop_resolution))

  if(!is.null(tot_pop_resolution)){
    profiled_sf <- sum_updated_pop_by_grp(profiled_sf = profiled_sf,
                                          grp_var_name = age_sex_var_name,
                                          nse_objs_ls = gen_objs_for_nse_upd_pop(tot_pop_resolution))
  }
  ## Add logic for NULL tot_pop_sf and tot_pop_resolution.
  ## Summarise results at group_by_var > age_sex_pop_resolution
  ## And / OR
  ## Summarise results at group_by_var

  ## OPTION 2: POP COUNTS BASED ON COMBINATION OF AGE/SEX and TOTAL POP RESOLUTION UNITS
  ## Summarise results at group_by_var > age_sex_pop_resolution
  ## And / OR
  ## Summarise results at group_by_var
  tot_pop_col <- paste0("year_",
                        data_year,
                        "pr")
  small_unit_sf <- dplyr::inner_join(tot_pop_sf,
                                     tot_pop_sf %>%
                                       sf::st_set_geometry(NULL) %>%
                                       dplyr::group_by(!!rlang::sym(age_sex_var_name)) %>% #SA2MAIN16 ## NEEDS TO CHANGE
                                       dplyr::summarise(whole_res_unit_all_age_pop = sum(!!rlang::sym(tot_pop_col)), #year_2016pr
                                                        age_sex_res_area_sqkm = sum(AREASQKM16)) %>%
                                       dplyr::ungroup())
  small_unit_sf <- small_unit_sf %>%
    dplyr::rename(tot_pop_res_area_sqkm = AREASQKM16) %>%
    dplyr::mutate(tot_pop_res_prop_of_age_sex_pop = !!rlang::sym(tot_pop_col) / whole_res_unit_all_age_pop,
                  tot_pop_res_prop_of_age_sex_res_area = tot_pop_res_area_sqkm / age_sex_res_area_sqkm) %>%
    dplyr::mutate(tot_pop_res_prop_of_age_sex_pop = replace(tot_pop_res_prop_of_age_sex_pop,is.nan(tot_pop_res_prop_of_age_sex_pop),0)) %>%
    dplyr::mutate(tot_pop_res_prop_of_age_sex_res_area = pmin(tot_pop_res_prop_of_age_sex_res_area,1))

  fine_grained_profiled_sf <- profiled_sf %>%
    dplyr::rename(age_sex_res_area_sqkm_check = AREASQKM16) %>%
    dplyr::mutate(included_age_sex_res_area_sqkm = sf::st_area(.) %>%
                    as.numeric()) %>%
    dplyr::mutate(included_age_sex_res_area_sqkm = included_age_sex_res_area_sqkm / 1000000) %>%
    dplyr::mutate(included_age_sex_res_area_prop = included_age_sex_res_area_sqkm / age_sex_res_area_sqkm_check)  %>%
    dplyr::mutate(included_age_sex_res_area_prop = pmin(included_age_sex_res_area_sqkm,1))

  ## MAKE CONDITIONAL ON TOT POP RESOLUTION BEING HIGHER
  fine_grained_profiled_sf <- intersect_sfs_keep_counts(profiled_sf = fine_grained_profiled_sf,
                                                            profiled_colref  = NA,
                                                            profiled_rowref = NA,
                                                            attribute_sf = small_unit_sf)
  fine_grained_profiled_sf <- fine_grained_profiled_sf %>%
    dplyr::mutate(included_tot_pop_res_area_sqkm = sf::st_area(.) %>%
                    as.numeric()) %>%
    dplyr::mutate(included_tot_pop_res_area_sqkm = included_tot_pop_res_area_sqkm / 1000000) %>%
    dplyr::mutate(included_tot_pop_res_area_prop = included_tot_pop_res_area_sqkm / tot_pop_res_area_sqkm)
  duplicate_names <- paste0(names(fine_grained_profiled_sf),".1")[paste0(names(fine_grained_profiled_sf),".1") %in%
                                                                    names(fine_grained_profiled_sf)]
  fine_grained_profiled_sf <- fine_grained_profiled_sf %>%
    dplyr::select(-duplicate_names) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("Female"),
                                 dplyr::contains("Male"),
                                 dplyr::contains("total")),
                     #dplyr::funs(list(a = paste0(.,"_sa2_as_whole"))))
                     dplyr::funs(paste0(age_sex_pop_resolution,"_complete_",
                                             #sub(paste0("_",age_sex_pop_resolution,"_complete"),"",
                                                  .)))
  #)
  fine_grained_profiled_sf <- fine_grained_profiled_sf %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("Female"),
                                 dplyr::contains("Male"),
                                 dplyr::contains("total")),
                     dplyr::funs(tpr_included =.*included_tot_pop_res_area_prop*tot_pop_res_prop_of_age_sex_pop))  %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("_tpr_included")),
                     dplyr::funs(paste0(tot_pop_resolution,
                                        "_included_",
                                              gsub("_tpr_included","",.)) %>%
                                   gsub(paste0(age_sex_pop_resolution,"_complete_"),"",.)))
  if(group_by_var==tot_pop_resolution){
    resolution_sf <- fine_grained_profiled_sf
  }else{
    resolution_sf <- profiled_sf
  }
  fine_grained_profiled_sf <- fine_grained_profiled_sf  %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::group_by(!!rlang::sym(age_sex_var_name)) %>%
    dplyr::summarise_at(dplyr::vars(dplyr::starts_with(paste0(tot_pop_resolution,
                                                              "_included_"))),
                        dplyr::funs(asr_included = sum(.))) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("asr_included")),
                     dplyr::funs(paste0("asr_included_",
                                        gsub("_asr_included"
                                             ,"",
                                             .)) %>%
                                   gsub(paste0(tot_pop_resolution,
                                               "_included_"),"",.)))

  if(group_by_var==tot_pop_resolution){
    fine_grained_profiled_sf <- fine_grained_profiled_sf %>%
      dplyr::ungroup()
  }
  resolution_sf <-  dplyr::inner_join(resolution_sf,
                                      fine_grained_profiled_sf)

  return(resolution_sf)
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
                                   tot_pop_col = NULL){
  # area_whl_unit <- paste0("area_whl_",sp_unit)
  # area_inc_unit <- paste0("area_inc_",sp_unit)
  # prop_inc_unit <- paste0("prop_inc_",sp_unit)
  # popl_inc_unit <- paste0("popl_inc_",sp_unit)
  # popl_whl_unit <- paste0("popl_whl_",sp_unit)
  nse_objs_ls <- gen_objs_for_nse_upd_pop(sp_unit = sp_unit)
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$area_inc_unit) := sf::st_area(.) %>% units::set_units(km^2))
  profiled_sf <- profiled_sf %>%
    dplyr::mutate(!!rlang::sym(nse_objs_ls$prop_inc_unit) := as.numeric(!!rlang::sym(nse_objs_ls$area_inc_unit)/!!rlang::sym(nse_objs_ls$area_whl_unit)))
  pop_prop_multiplier <- nse_objs_ls$prop_inc_unit
  if(!is.null(tot_pop_col)){
    prop_ttl_pop<- paste0("prop_tlp_",sp_unit)
    popl_inc_unit <- paste0(nse_objs_ls$popl_inc_unit,"_",tot_pop_col)
    profiled_sf <- profiled_sf %>%
      dplyr::mutate(!!rlang::sym(popl_inc_unit) :=!!rlang::sym(tot_pop_col)*!!rlang::sym(nse_objs_ls$prop_inc_unit)) %>%
      dplyr::mutate(!!rlang::sym(prop_ttl_pop) := !!rlang::sym(popl_inc_unit)/!!rlang::sym(tot_pop_col))
    pop_prop_multiplier <- prop_ttl_pop
  }

  profiled_sf <- profiled_sf %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(paste0("y",data_year,".Females.")),
                                 dplyr::starts_with(paste0("y",data_year,".Males."))),
                     dplyr::funs(!!rlang::sym(nse_objs_ls$popl_inc_unit) :=.*!!rlang::sym(pop_prop_multiplier))) %>%
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
                                   nse_objs_ls){
  profiled_sf %>%
    dplyr::inner_join(.,profiled_sf %>%
                        sf::st_set_geometry(NULL) %>%
                        dplyr::group_by(!!rlang::sym(grp_var_name)) %>%
                        dplyr::summarise_at(dplyr::vars(dplyr::starts_with(nse_objs_ls$popl_inc_unit)),
                                            dplyr::funs(grpd = sum(.))) %>%
                        suffix_to_prefix(suffix = "grpd") %>%
                        dplyr::ungroup())
}
#s1 missing
