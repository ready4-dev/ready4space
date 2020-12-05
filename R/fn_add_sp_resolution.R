#' @title add_sp_resolution
#' @description FUNCTION_DESCRIPTION
#' @param lookup_tbs_r4 PARAM_DESCRIPTION
#' @param processed_dir PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{arrange}}
#'  \code{\link[stringr]{str_subset}}
#'  \code{\link[purrr]{map2}}
#'  \code{\link[tibble]{tibble}}
#' @rdname add_sp_resolution
#' @export
#' @importFrom dplyr filter select mutate pull arrange
#' @importFrom stringr str_which
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
add_sp_resolution <- function(lookup_tbs_r4,
                              processed_dir){
  dr_dp_tb <- sp_data_pack_lup(lookup_tbs_r4) %>%
    dplyr::filter(main_feature == "Boundary") %>%
    dplyr::select(area_type,country,region,source_reference,year) %>%
    dplyr::mutate(source_reference = paste0(processed_dir,
                                            "/",
                                            source_reference,
                                            ".rds"))
  dr_dp_vec <- dr_dp_tb  %>%
    dplyr::pull(source_reference)
  dr_nt_vec <- dr_dp_tb  %>%
    dplyr::pull(region)
  if(any(dr_nt_vec=="National")){
    nat_sf <- readRDS(dr_dp_vec[stringr::str_which(dr_nt_vec,"National") %>% min()])
    nat_area <- nat_sf %>% get_area_sqkm_sf()
  }else{
    nat_area <- NA_real_
  }
  resolution_lup_r3 <- purrr::pmap_dfr(dr_dp_tb,
                                           ~ tibble::tibble(parent_area = ..2,
                                                            boundary_year = as.numeric(..5),
                                                            area_type = ..1,
                                                            area_count = nrow(readRDS(..4)) %>% as.double(),
                                                            complete = T,
                                                            summed_area = ifelse(..3=="National",
                                                                                 nat_area,
                                                                                 readRDS(..4) %>% get_area_sqkm_sf()),
                                                            mean_size = summed_area / area_count))
  resolution_lup_r3 <- resolution_lup_r3 %>%
    ready4_sp_resolution_lup() %>%
    dplyr::arrange(mean_size)
  `sp_resolution_lup<-`(lookup_tbs_r4, resolution_lup_r3)
}
#' @title get_area_sqkm_sf
#' @description FUNCTION_DESCRIPTION
#' @param data_sf PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{pull}}
#'  \code{\link[sf]{geos_measures}}
#'  \code{\link[units]{set_units}}
#' @rdname get_area_sqkm_sf
#' @export
#' @importFrom dplyr mutate summarise pull
#' @importFrom sf st_area
#' @importFrom units set_units
get_area_sqkm_sf <- function(data_sf){
  data_sf %>%
    dplyr::mutate(FT_AREA_SQKM = sf::st_area(.) %>%
                    units::set_units(km^2)) %>%
    dplyr::summarise(TOT_AREA_SQKM = sum(FT_AREA_SQKM)) %>%
    dplyr::pull(TOT_AREA_SQKM)
}
