#' @title
#' Adds area attributes to a spatial object.
#'
#' @description
#' Merges a SF file with population data relating to areas described in that file.
#'
#' @details  Makes data transformations (variable name, changes from strings to factors) necessary
#' to ensure that merged objects are compatible.
#'
#' @param country A String naming the country to which data applies
#' @param state A String naming the State to which data applies
#' @param area_unit A String specifying that the area type is either "LGA" or "SA2".
#' @param boundary_year A String specifying the year to which boundary data applies
#' @param attribute_data A String vector, specifying the names of the files from which attribute data will
#' be taken.
#' @param data_lookup_tb A tibble, with lookup values for all data input files.
#'
#'
#' @return
#' Returns a SF.
#'
#' @export
#'
#' @examples
#'
recur_add_attr_to_sf <- function(country,
                                 state = NULL,
                                 area_unit,
                                 boundary_year,
                                 attribute_data,
                                 data_lookup_tb = aus_spatial_lookup_tb){
  data_lookup_tb <- data_lookup_tb %>%
    dplyr::filter(area_type == area_unit) %>%
    dplyr::filter(year == boundary_year)
  boundary_file <- ready.data::data_get(data_lookup_tb = data_lookup_tb,
                                       lookup_reference = "Boundary",
                                       lookup_variable = "main_feature",
                                       target_variable = "source_reference")
  if(!is.null(state)){
    if(country=="Australia")
      state_var_name <- paste0("STE_NAME",stringr::str_sub(boundary_year,3,4))
    boundary_file <- boundary_file %>%
      dplyr::filter(!!rlang::sym(state_var_name) == state)
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
                                      boundary_year = boundary_year))
}
##
add_attr_list_to_sf <- function(x,
                                y,
                                area_unit,
                                boundary_year){
  add_attr_to_sf(area_unit = area_unit,
                 area_sf = x,
                 attr_data_tb = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                     lookup_reference = y,
                                                     lookup_variable = "name",
                                                     target_variable = "source_reference"),
                 attr_data_desc = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                       lookup_reference = y,
                                                       lookup_variable = "name",
                                                       target_variable = "main_feature",
                                                       evaluate = FALSE),
                 attr_data_year = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                       lookup_reference = y,
                                                       lookup_variable = "name",
                                                       target_variable = "year",
                                                       evaluate = FALSE),
                 boundary_year = boundary_year)
}
##
add_attr_to_sf <- function(area_unit,
                           area_sf,
                           attr_data_tb,
                           attr_data_desc,
                           attr_data_year,
                           boundary_year = "2016"){
  if(attr_data_desc == "Population projections"){
    attr_data_tb <- prepare_pop_preds_data(attr_data_tb = attr_data_tb,
                                             attr_data_year = attr_data_year,
                                             area_unit = area_unit,
                                             boundary_year = boundary_year)
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  if(stringr::str_detect(attr_data_desc, "ERP by age and sex")){
    attr_data_tb <- prepare_child_youth_data(child_youth_data = attr_data_tb,
                                                     area_unit = area_unit,
                                                     #attr_data_year = attr_data_year,
                                                     boundary_year = boundary_year)
      merged_units <- dplyr::inner_join(area_sf,
                                        attr_data_tb) %>%
        sf::st_as_sf()
  }
  if(attr_data_desc == "ERP"){
    attr_data_tb <- prepare_erp_data(erp_data = attr_data_tb,
                                 area_unit = area_unit,
                                 boundary_year = boundary_year)
    merged_units <- dplyr::inner_join(area_sf,
                                      attr_data_tb) %>%
      sf::st_as_sf()
  }
  if(attr_data_desc == "SEIFA"){
    t1_stub <- stringr::str_sub(boundary_year,start=3,end=4)
    attr_data_tb <- prepare_seifa_data(seifa_data = attr_data_tb,
                                               area_unit = area_unit,
                                               #attr_data_year = attr_data_year,
                                               t1_stub = t1_stub)
    if(area_unit == "LGA"){
      groupvar <- rlang::sym(paste0("LGA_CODE",t1_stub))
      unitname <- paste0("LGA_NAME",t1_stub)
    }
    if(area_unit=="SA2"){
      groupvar <- rlang::sym(paste0("SA2_MAIN",t1_stub))
      unitname <- paste0("SA2_NAME",t1_stub)
    }
    merged_units <- dplyr::left_join(area_sf,
                                     attr_data_tb)
    merged_units <- dplyr::inner_join(merged_units,
                                      merged_units %>%
                                        dplyr::group_by(!!groupvar) %>%
                                        dplyr::summarise(resident.pop.all.parts=sum(Usual.Res.Pop)) %>%
                                        dplyr::ungroup() %>%
                                        sf::st_set_geometry(NULL))
    merged_units <- dplyr::inner_join(merged_units,
                                      summarise_seifa(seifa_sf = merged_units,
                                                      groupvar = groupvar,
                                                      unitname = unitname) %>%
                                        sf::st_set_geometry(NULL)) %>%
      dplyr::select(-c("Usual.Res.Pop",
                        "Score",
                        "Rank.Australia",
                        "Decile.Australia",
                        "Percentile.Australia",
                        "State",
                        "Rank.ST",
                        "Decile.ST",
                        "Percentile.ST",
                        "Minimum score for SA1s in area",
                        "Maximum score for SA1s in area",
                        "% Usual Resident Population without an SA1 level score",
                        "resident.pop.all.parts"))
  }##
  return(merged_units)
}
prepare_pop_preds_data <- function(attr_data_tb,
                                   attr_data_year,
                                   area_unit,
                                   boundary_year){
  t1_stub <- stringr::str_sub(boundary_year,start=3,end=4)
  pop_preds_data <- attr_data_tb
  if(area_unit=="LGA"){
    pop_preds_data <- pop_preds_data %>%
      dplyr::mutate(`Local Government Area` = ifelse(`Local Government Area` =="Kingston (C)","Kingston (C) (Vic.)",`Local Government Area`),
                    `Local Government Area` = ifelse(`Local Government Area` =="Latrobe (C)","Latrobe (C) (Vic.)",`Local Government Area`),
                    `Local Government Area` = ifelse(`Local Government Area` =="Wodonga (RC)","Wodonga (C)",`Local Government Area`))

    pop_preds_data[["LGA Code"]] <- factor(pop_preds_data[["LGA Code"]])
    pop_preds_data[["Local Government Area"]] <- factor(pop_preds_data[["Local Government Area"]])
    pop_preds_data <- pop_preds_data %>%
      dplyr::rename(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := "LGA Code") %>%
      dplyr::rename(!!rlang::sym(paste0("LGA_NAME",t1_stub)) := "Local Government Area")
    pop_preds_data <- spatial_select_rename_age_sex(pop_preds_data,
                                                    attr_data_year,
                                                    also_include = c(paste0("LGA_CODE",t1_stub),
                                                                     paste0("LGA_NAME",t1_stub)))
  }
  return(pop_preds_data)
}
#
prepare_erp_data <- function(erp_data,
                             area_unit,
                             boundary_year){
  t1_stub <- stringr::str_sub(boundary_year,start=3,end=4)
  if(area_unit =="SA1"){
    erp_data[["SA1"]] <- factor(as.character(erp_data[["SA1"]]))
    erp_data <- erp_data %>%
      dplyr::rename(!!rlang::sym(paste0("SA1_7DIG",t1_stub)) := "SA1")
  }
  return(erp_data)
}
prepare_child_youth_data <- function(child_youth_data,
                                     area_unit,
                                     #attr_data_year,
                                     boundary_year){
  #t0 <- attr_data_year
  t1_stub <- stringr::str_sub(boundary_year,start=3,end=4)
  child_youth_pop_data <- child_youth_data
  if(area_unit=="LGA"){
    child_youth_pop_data[["LGA code"]] <- factor(child_youth_pop_data[["LGA code"]])
    child_youth_pop_data[["LGA name"]] <- factor(child_youth_pop_data[["LGA name"]])
  }
  if(area_unit=="SA2"){
    child_youth_pop_data[["SA2 code"]] <- factor(child_youth_pop_data[["SA2 code"]])
    child_youth_pop_data[[paste0("SA2_MAIN",t1_stub)]] <- factor(child_youth_pop_data[[paste0("SA2_MAIN",t1_stub)]])
    child_youth_pop_data[["SA2 name"]] <- factor(child_youth_pop_data[["SA2 name"]])
    child_youth_pop_data[["SA3 code"]] <- factor(child_youth_pop_data[["SA3 code"]])
    child_youth_pop_data[["SA3 name"]] <- factor(child_youth_pop_data[["SA3 name"]])
    child_youth_pop_data[["SA4 code"]] <- factor(child_youth_pop_data[["SA4 code"]])
    child_youth_pop_data[["SA4 name"]] <- factor(child_youth_pop_data[["SA4 name"]])
  }
  child_youth_pop_data[["S/T code"]] <- factor(child_youth_pop_data[["S/T code"]])
  child_youth_pop_data[["S/T name"]] <- factor(child_youth_pop_data[["S/T name"]])

  if(area_unit=="LGA"){
    child_youth_pop_data <- child_youth_pop_data %>%
      dplyr::rename(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := "LGA code",
                    !!rlang::sym(paste0("LGA_NAME",t1_stub)) := "LGA name",
                    !!rlang::sym(paste0("STE_CODE",t1_stub)) := "S/T code",
                    !!rlang::sym(paste0("STE_NAME",t1_stub)) := "S/T name"
      )
  }
  if(area_unit=="SA2"){
    child_youth_pop_data <- child_youth_pop_data %>%
      dplyr::rename(!!rlang::sym(paste0("SA2_MAIN",t1_stub)) := paste0("SA2_MAIN",t1_stub),
                    !!rlang::sym(paste0("SA2_5DIG",t1_stub)) := "SA2 code",
                    !!rlang::sym(paste0("SA2_NAME",t1_stub)) := "SA2 name",
                    !!rlang::sym(paste0("STE_CODE",t1_stub)) := "S/T code",
                    !!rlang::sym(paste0("STE_NAME",t1_stub)) := "S/T name",
                    !!rlang::sym(paste0("GCC_CODE",t1_stub)) := "GCCSA code",
                    !!rlang::sym(paste0("GCC_NAME",t1_stub)) := "GCCSA name",
                    !!rlang::sym(paste0("SA3_NAME",t1_stub)) := "SA3 name",
                    !!rlang::sym(paste0("SA3_CODE",t1_stub)) := "SA3 code",
                    !!rlang::sym(paste0("SA4_NAME",t1_stub)) := "SA4 name",
                    !!rlang::sym(paste0("SA4_CODE",t1_stub)) := "SA4 code")
  }
  t0 <- names(child_youth_pop_data) %>% stringr::str_subset("Females.15.19") %>% stringr::str_sub(2,5)
  f.total0to14.t0 <- paste0("y",t0,".total0to14f")
  m.total0to14.t0 <- paste0("y",t0,".total0to14m")
  p.total0to14.t0 <- paste0("y",t0,".total0to14p")
  f.total15to24.t0 <- paste0("y",t0,".total15to24f")
  m.total15to24.t0 <- paste0("y",t0,".total15to24m")
  p.total15to24.t0 <- paste0("y",t0,".total15to24p")
  child_youth_pop_data <- child_youth_pop_data %>%
    dplyr::mutate(rlang::UQ(p.total0to14.t0) := !!rlang::sym(f.total0to14.t0) + !!rlang::sym(m.total0to14.t0),
                                        rlang::UQ(p.total15to24.t0) := !!rlang::sym(f.total15to24.t0) + !!rlang::sym(m.total15to24.t0))
  return(child_youth_pop_data)
}
#
prepare_seifa_data <- function(seifa_data,
                               area_unit,
                               #attr_data_year,
                               t1_stub){
  #t1_stub <- stringr::str_sub(boundary_year,start=3,end=4)
  if(area_unit=="LGA"){
    seifa_deciles_by_unit <- seifa_data %>%
      dplyr::select(-"State") %>%
      dplyr::rename(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := "LGA_Code",
                    !!rlang::sym(paste0("LGA_NAME",t1_stub)) := "LGA_NAME") %>%
      dplyr::mutate(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := base::as.character(!!rlang::sym(paste0("LGA_CODE",t1_stub))))
  }
  if(area_unit=="SA2"){
    seifa_deciles_by_unit <- seifa_data %>%
      dplyr::rename(SA2_MAIN16=SA2_Code,
                    SA2_NAME16=SA2_NAME)
    seifa_deciles_by_unit[[paste0("SA2_MAIN",t1_stub)]] <- base::factor(seifa_deciles_by_unit[[paste0("SA2_MAIN",t1_stub)]])
  }
  return(seifa_deciles_by_unit)
}
#
summarise_seifa <- function(seifa_sf,
                            groupvar,
                            unitname){
  unitnamesym <- rlang::sym(unitname)
  seifa_sf <- seifa_sf %>%
    dplyr::mutate(part.pop.weighting=Usual.Res.Pop/resident.pop.all.parts) %>%
    dplyr::mutate(Percentile.Australia=Percentile.Australia*part.pop.weighting) %>%
    dplyr::group_by(!!groupvar) %>%
    dplyr::summarize(!! unitname := dplyr::first((!!unitnamesym)),
                     STE_NAME16 = dplyr::first(STE_NAME16),
                     AREASQKM16 = mean(AREASQKM16),
                     seifa.percentile = sum(Percentile.Australia))
  return(seifa_sf)
}
##

##

