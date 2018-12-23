#' @title Get population counts from age 0-30 by SA2 for a given state / territory.
#' @description FUNCTION_DESCRIPTION
#' @param s_t PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[ready.data]{data_get}}
#'  \code{\link[dplyr]{filter}}
#' @rdname state_territory_sa2_pop_counts_0_30
#' @export
#' @importFrom ready.data data_get
#' @importFrom dplyr filter

state_territory_sa2_pop_counts_0_30 <- function(s_t){
  ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                       lookup_reference = "aus_age_sex_seifa_sa2s_sf",
                       lookup_variable = "name",
                       target_variable = "source_reference")%>%
    dplyr::filter(STE_NAME16 == s_t)
}


#' @describeIn state_territory_sa2_pop_counts_0_30 Calculate ...
state_territory_sa1_pop_counts_projs <- function(s_t){
  merge(ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                   lookup_reference = "aus_sa1_nat_shp_bound_2016",
                                   lookup_variable = "name",
                                   target_variable = "source_reference") %>%
                dplyr::filter(STE_NAME16 == s_t),
              ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                   lookup_reference = "aus_sa1_nat_att_erp_2017",
                                   lookup_variable = "name",
                                   target_variable = "source_reference"),
              by.x = base::c("SA1_7DIG16"),
              by.y = base::c("SA1"),
              all = FALSE)
}

