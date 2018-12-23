#' @title
#' Read in Victorian 2016 to 2021 population growth data
#'
#' @description
#' This function creates a tibble of projected population growth by Victorian LGA
#' between 2016 and 2021.
#'
#' @family spatial functions.
#'
#' @details
#'
#' @param vic_pop_growth_by_age_lga_t0 PARAM_DESCRIPTION, Default: ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#'    lookup_reference = "aus_lga_vic_att_ppr_2016", lookup_variable = "name",
#'    target_variable = "source_reference") %>% purrr::pluck("y2016")
#' @param vic_pop_growth_by_age_lga_t1 PARAM_DESCRIPTION, Default: ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#'    lookup_reference = "aus_lga_vic_att_ppr_2016", lookup_variable = "name",
#'    target_variable = "source_reference") %>% purrr::pluck("y2021"), Default: '2016'
#'
#' @param t0 A String specifying the baseline year. This must match up with the years
#' specified in the datasource specified in vic_pop_growth_by_age_lga_t0 (spaced at
#' five year intervals: "2016", "2021", "2026", etc).
#'
#' @param t1 A String specifying the baseline year. This must match up with the years
#' specified in the datasource specified in vic_pop_growth_by_age_lga_t1 (spaced at
#' five year intervals: "2016", "2021", "2026", etc), Default: '2021'
#'
#' @return
#' A tibble.

#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready.data]{ready.data::data_get}}
#'  \code{\link[purrr]{pluck}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{join}}
#' @rdname spatial_vic_pop_growth_lga
#' @export
#' @importFrom ready.data ready.data::data_get
#' @importFrom purrr pluck
#' @importFrom dplyr rename inner_join

spatial_vic_pop_growth_lga <- function(vic_pop_growth_by_age_lga_t0 = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                           lookup_reference = "aus_lga_vic_att_ppr_2016",
                                                                                           lookup_variable = "name",
                                                                                           target_variable = "source_reference") %>%
                                         purrr::pluck("y2016"),
                                       vic_pop_growth_by_age_lga_t1 = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                           lookup_reference = "aus_lga_vic_att_ppr_2016",
                                                                                           lookup_variable = "name",
                                                                                           target_variable = "source_reference") %>%
                                         purrr::pluck("y2021"),
                                       t0 ="2016",
                                       t1 ="2021"){
  vic_pop_growth_by_age_lga_t0 <- spatial_select_rename_age_sex(population_tib = vic_pop_growth_by_age_lga_t0,
                                                                year = t0,
                                                                also_include = c("LGA Code",
                                                                                 "Local Government Area")) %>%
    dplyr::rename("LGA" = "Local.Government.Area")
  vic_pop_growth_by_age_lga_t1 <- spatial_select_rename_age_sex(population_tib = vic_pop_growth_by_age_lga_t1,
                                                                year = t1,
                                                                also_include = c("LGA Code",
                                                                                 "Local Government Area")) %>%
    dplyr::rename("LGA" = "Local.Government.Area")
  vic.pop.growth.by.age.lga <- dplyr::inner_join(vic_pop_growth_by_age_lga_t0,
                                                 vic_pop_growth_by_age_lga_t1,
                                                 by = c("LGA.Code", "LGA"))
  vic.pop.growth.by.age.lga <- spatial_population_growth(population_tib = vic.pop.growth.by.age.lga,
                                                         t0 = t0,
                                                         t1 = t1)
  vic.pop.growth.by.age.lga <- vic.pop.growth.by.age.lga %>%
    dplyr::rename(LGA_NAME16=LGA)
  return(vic.pop.growth.by.age.lga)
}
