#' @title
#' Create tibble of population data for all spatial units of a specified type (currently Australian LGA and SA2).
#'
#' @description
#' Sources and merges population and SEIFA data for all spatial units of a specified type.
#'
#' @family spatial functions.
#'
#' @details  Makes data transformations (variable name, changes from strings to factors) necessary
#' to ensure that merged objects are compatible.
#'
#' @param area_unit A String specifying that the area type is either "LGA" or "SA2".
#' @param area_sf A SF opject, with the boundary geometeries of the specified spatial unit type.
#' @param child_youth_pop_t0_tb A tibble of age and sex population data for each area unit specified above
#' for timepoint 0.
#' @param child_youth_pop_t1_tb A tibble of age and sex population data for each area unit specified above
#' for timepoint 1.
#' @param seifa_deciles_by_unit A tibble of SEIFA data for each area unit specified above.
#'
#' @return
#' Returns a tibble.
#'
#' @export
#'
#' @examples
#'
spatial_merge_areas_attributes<-function(area_unit,
                                         area_sf,
                                         child_youth_pop_t0_tb,
                                         child_youth_pop_t1_tb,
                                         seifa_deciles_by_unit
){
  t0 <- child_youth_pop_t0_tb %>%
    base::names() %>%
    stringr::str_subset("Females.25.29") %>%
    stringr::str_replace(".Females.25.29","") %>%
    stringr::str_replace("y","")
  t1 <-  child_youth_pop_t1_tb %>%
    base::names() %>%
    stringr::str_subset("Females.25.29") %>%
    stringr::str_replace(".Females.25.29","") %>%
    stringr::str_replace("y","")
  t1_stub<-stringr::str_sub(t1,start=3,end=4)
  if(area_unit=="LGA"){
    t0_t1_LGA_name <- rlang::sym(paste0("LGA_NAME_",t1))
    merged_young_pop <- dplyr::inner_join(child_youth_pop_t1_tb,
                                          child_youth_pop_t0_tb %>%
                                            dplyr::rename(`LGA name` = !!t0_t1_LGA_name)
    )
  }
  if(area_unit=="SA2"){
    merged_young_pop <- dplyr::inner_join(child_youth_pop_t1_tb,
                                          child_youth_pop_t0_tb,
                                          by = c("SA3 code",
                                                 "SA3 name",
                                                 "SA2 code",
                                                 "SA2 name",
                                                 "S/T code",
                                                 "S/T name",
                                                 "GCCSA code",
                                                 "GCCSA name",
                                                 "SA4 code",
                                                 "SA4 name",
                                                 paste0("SA2_MAIN",t1_stub)))
  }
  if(area_unit=="LGA"){
    merged_young_pop[["LGA code"]] <- base::factor(merged_young_pop[["LGA code"]])
    merged_young_pop[["LGA name"]] <- base::factor(merged_young_pop[["LGA name"]])
  }
  if(area_unit=="SA2"){
    merged_young_pop[["SA2 code"]] <- base::factor(merged_young_pop[["SA2 code"]])
    merged_young_pop[[paste0("SA2_MAIN",t1_stub)]] <- base::factor(merged_young_pop[[paste0("SA2_MAIN",t1_stub)]])
    merged_young_pop[["SA2 name"]] <- base::factor(merged_young_pop[["SA2 name"]])
  }
  merged_young_pop[["S/T code"]] <- base::factor(merged_young_pop[["S/T code"]])
  merged_young_pop[["S/T name"]] <- base::factor(merged_young_pop[["S/T name"]])

  if(area_unit=="LGA"){
    merged_young_pop <- merged_young_pop %>%
      dplyr::rename(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := "LGA code",
                    !!rlang::sym(paste0("LGA_NAME",t1_stub)) := "LGA name",
                    !!rlang::sym(paste0("STE_CODE",t1_stub)) := "S/T code",
                    !!rlang::sym(paste0("STE_NAME",t1_stub)) := "S/T name"
      )
    merged.lgas <- dplyr::inner_join(area_sf,
                                     merged_young_pop
    ) %>%
      sf::st_as_sf()
  }
  if(area_unit=="SA2"){
    merged_young_pop <- merged_young_pop %>%
      dplyr::rename(!!rlang::sym(paste0("SA2_MAIN",t1_stub)) := paste0("SA2_MAIN",t1_stub),
                    !!rlang::sym(paste0("SA2_5DIG",t1_stub)) := "SA2 code",
                    !!rlang::sym(paste0("SA2_NAME",t1_stub)) := "SA2 name",
                    !!rlang::sym(paste0("STE_CODE",t1_stub)) := "S/T code",
                    !!rlang::sym(paste0("STE_NAME",t1_stub)) := "S/T name",
                    !!rlang::sym(paste0("GCC_CODE",t1_stub)) := "GCCSA code",
                    !!rlang::sym(paste0("GCC_NAME",t1_stub)) := "GCCSA name")
    merged_sa2s <- dplyr::inner_join(area_sf,
                                     merged_young_pop
    ) %>%
      sf::st_as_sf()
  }
  if(area_unit=="LGA"){
    #seifa_deciles_by_unit[["LGA code"]] <- base::factor(seifa_deciles_by_unit[["LGA code"]])
    #seifa_deciles_by_unit[["LGA name"]] <- base::factor(seifa_deciles_by_unit[["LGA name"]])
    seifa_deciles_by_unit <- seifa_deciles_by_unit %>%
      dplyr::select(-"State") %>%
      dplyr::rename(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := "LGA_Code",
                    !!rlang::sym(paste0("LGA_NAME",t1_stub)) := "LGA_NAME") %>%
      dplyr::mutate(!!rlang::sym(paste0("LGA_CODE",t1_stub)) := base::as.character(!!rlang::sym(paste0("LGA_CODE",t1_stub))))

    merged.lgas <-  dplyr::left_join(merged.lgas,
                                      seifa_deciles_by_unit
    )
    #base::setdiff(seifa_deciles_by_unit %>% dplyr::select(LGA_NAME) %>% dplyr::pull(),merged.lgas %>% sf::st_set_geometry(NULL) %>% dplyr::select(LGA_NAME16) %>% dplyr::pull())
    #base::setdiff(merged.lgas %>% sf::st_set_geometry(NULL) %>% dplyr::select(LGA_NAME16) %>% dplyr::pull(),seifa_deciles_by_unit %>% dplyr::select(LGA_NAME) %>% dplyr::pull())
    ## returns: "Maralinga Tjarutja (AC)"
  }
  if(area_unit=="SA2"){
    seifa_deciles_by_unit <- seifa_deciles_by_unit %>%
      dplyr::rename(SA2_MAIN16=SA2_Code,
                    SA2_NAME16=SA2_NAME)
    seifa_deciles_by_unit[[paste0("SA2_MAIN",t1_stub)]] <- base::factor(seifa_deciles_by_unit[[paste0("SA2_MAIN",t1_stub)]])
  }
  if(area_unit=="LGA"){
    groupvar <- rlang::sym(paste0("LGA_CODE",t1_stub))
    unitname <- paste0("LGA_NAME",t1_stub)
  }
  if(area_unit=="SA2"){
    groupvar <-rlang::sym(paste0("SA2_MAIN",t1_stub))
    unitname <-paste0("SA2_NAME",t1_stub)
  }
  if(area_unit=="LGA"){
    merged_units <- dplyr::inner_join(merged.lgas,
                                      merged.lgas %>%
                                        dplyr::group_by(!!groupvar) %>%
                                        dplyr::summarise(resident.pop.all.parts=sum(Usual.Res.Pop
                                        )
                                        ) %>%
                                        dplyr::ungroup() %>%
                                        sf::st_set_geometry(NULL)
    )
  }
  if(area_unit=="SA2"){
    merged_sa2s <- dplyr::left_join(merged_sa2s,
                                    seifa_deciles_by_unit)
    merged_units <- dplyr::inner_join(merged_sa2s,
                                      merged_sa2s %>%
                                        dplyr::group_by(!!groupvar) %>%
                                        dplyr::summarise(resident.pop.all.parts=sum(Usual.Res.Pop
                                        )
                                        ) %>%
                                        dplyr::ungroup() %>%
                                        sf::st_set_geometry(NULL)
    )
  }
  ##
  merged_units <- spatial_summarise_population_counts_0to29(population_sf = merged_units,
                                                                        groupvar = groupvar,
                                                                        unitname = unitname,
                                                                        t0 = t0,
                                                                        t1 = t1)
  return(merged_units)
}
