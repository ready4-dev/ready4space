#' @title Calculate area within specified drives timeof a cluster of points.
#' @description FUNCTION_DESCRIPTION
#' @param long PARAM_DESCRIPTION
#' @param lat PARAM_DESCRIPTION
#' @param time_min PARAM_DESCRIPTION
#' @param time_max PARAM_DESCRIPTION
#' @param nbr_time_steps PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[osrm]{osrmIsochrone}}
#'  \code{\link[sf]{st_as_sf}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}}
#' @rdname spatial_area_within_xmin_drive_of_points
#' @export
#' @importFrom osrm osrmIsochrone
#' @importFrom sf st_as_sf
#' @importFrom dplyr mutate arrange

## Implements: https://rpubs.com/maiae/drivetime
spatial_area_within_xmin_drive_of_points <- function(long,
                                                     lat,
                                                     time_min,
                                                     time_max,
                                                     nbr_time_steps){
  time_step <- (time_max-time_min)/nbr_time_steps
  iso <- osrm::osrmIsochrone(loc = c(long, lat),
                             breaks = seq(from = time_min,
                                          to = time_max,
                                          by = time_step))
  iso_sf <- sf::st_as_sf(iso) %>%
    dplyr::mutate(drive_times = paste0(min,
                                       " to ",
                                       max,
                                       " mins")) %>%
    dplyr::arrange(id)

  return(iso_sf)
}

#' @describeIn spatial_area_within_xmin_drive_of_points Calculates the .....
#' @param cluster_tb PARAM_DESCRIPTION
#' @param service PARAM_DESCRIPTION
one_cluster_one_service_travel_time <- function(cluster_tb,
                                                service,
                                                time_min,
                                                time_max,
                                                nbr_time_steps){
  one_service <- cluster_tb %>%
    dplyr::filter(service_name == service)
  one_service_sf <- spatial_area_within_xmin_drive_of_points(long = one_service %>% dplyr::select(long) %>% dplyr::pull(),
                                                             lat = one_service %>% dplyr::select(lat) %>% dplyr::pull(),
                                                             time_min = time_min,
                                                             time_max = time_max,
                                                             nbr_time_steps = nbr_time_steps)
  return(one_service_sf)
}

#' @describeIn spatial_area_within_xmin_drive_of_points Calculates the .....
#' @param look_up_ref PARAM_DESCRIPTION
#' @param one_cluster_travel_time_sf_list PARAM_DESCRIPTION
one_service_time_bands <- function(look_up_ref,
                                   one_cluster_travel_time_sf_list){
  travel_time_bands <- one_cluster_travel_time_sf_list %>%
    purrr::pluck(look_up_ref) %>% dplyr::pull(drive_times)
  time_band_sf_list <- purrr::map(travel_time_bands,
                                  ~ one_cluster_travel_time_sf_list %>%
                                    purrr::pluck(look_up_ref) %>%
                                    dplyr::filter(drive_times == .x)) %>%
    stats::setNames(paste0("tb_"
                           ,stringr::str_replace_all(travel_time_bands,
                                                     " ",
                                                     "_")))
}

#' @describeIn spatial_area_within_xmin_drive_of_points Calculates the .....
#' @param time_band_ref PARAM_DESCRIPTION
#' @param one_cluster_time_bands_list PARAM_DESCRIPTION
union_one_travel_time_band_across_sites <- function(time_band_ref,
                                                    one_cluster_time_bands_list){
  list_of_new_sfs <- purrr::map(one_cluster_time_bands_list,
                                ~ .x %>% purrr::pluck(time_band_ref))
  purrr::reduce(list_of_new_sfs,
                ~ sf::st_union(.x,.y)  %>%
                  dplyr::select(id,min,max,center,drive_times))
}

#' @describeIn spatial_area_within_xmin_drive_of_points Calculates the .....
#' @param look_up_ref PARAM_DESCRIPTION
#' @param one_cluster_up_to_xmin_list PARAM_DESCRIPTION
fix_names_up_to_sfs <- function(look_up_ref,
                                one_cluster_up_to_xmin_list){
  max_var <- "max"
  max_vec <- one_cluster_up_to_xmin_list %>%
    purrr::pluck(look_up_ref) %>%
    base::names() %>%
    stringr::str_subset("max")
  if(length(max_vec) > 1){
    max_var <- max_vec %>%
      stringr::str_subset("max.") %>%
      stringr::str_replace_all("max.","") %>%
      base::as.numeric() %>%
      base::max() %>%
      paste0("max.",.)
  }
  return_object <- one_cluster_up_to_xmin_list %>%
    purrr::pluck(look_up_ref) %>%
    dplyr::mutate(max := !!rlang::sym(max_var)) %>%
    dplyr::mutate(center = (min + max) / 2) %>%
    dplyr::mutate(drive_times = paste0("0 to ",max," mins")) %>%
    dplyr::select(id,min,max,center,drive_times)
  return(return_object)
}

#' @describeIn spatial_area_within_xmin_drive_of_points Calculates the .....
#' @param cluster_tbs_list PARAM_DESCRIPTION
#' @param look_up_ref PARAM_DESCRIPTION
#' @export
cluster_isochrones <- function(cluster_tbs_list,
                               look_up_ref,
                               time_min = 0,
                               time_max = 60,
                               nbr_time_steps = 5){
  ##
  require(osrm)
  one_cluster_services_vec <- cluster_tbs_list %>%
    purrr::pluck(look_up_ref) %>%
    dplyr::select(service_name) %>%
    dplyr::pull()
  ##
  cluster_tb = cluster_tbs_list %>%
    purrr::pluck(look_up_ref)
  ##
  one_cluster_travel_time_sf_list <- purrr::map(one_cluster_services_vec,
                                                ~ one_cluster_one_service_travel_time(cluster_tb = cluster_tb,
                                                                                      service = .x,
                                                                                      time_min = time_min,
                                                                                      time_max = time_max,
                                                                                      nbr_time_steps = nbr_time_steps)) %>%
    stats::setNames(., one_cluster_services_vec)
  detach("package:osrm", unload=TRUE)
  ##
  one_cluster_time_bands_list <- purrr::map(1:length(one_cluster_travel_time_sf_list),
                                            ~ one_service_time_bands(look_up_ref = .x,
                                                                     one_cluster_travel_time_sf_list = one_cluster_travel_time_sf_list)) %>%
    stats::setNames(one_cluster_travel_time_sf_list %>% base::names())
  ##
  one_cluster_unioned_time_bands_list <- purrr::map(1:(one_cluster_time_bands_list %>%
                                                         purrr::pluck(1) %>%
                                                         base::length()),
                                                    ~ union_one_travel_time_band_across_sites(time_band_ref = .x,
                                                                                              one_cluster_time_bands_list =  one_cluster_time_bands_list)
  ) %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_time_bands_list %>%
                                 purrr::pluck(1) %>%
                                 base::length())))
  ##
  one_cluster_up_to_xmin_list <- purrr::accumulate(one_cluster_unioned_time_bands_list,
                                                   ~ sf::st_union(.x,.y))  %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_unioned_time_bands_list %>%
                                 base::length())))
  ##
  one_cluster_up_to_xmin_list <- purrr::map(1:length(one_cluster_up_to_xmin_list),
                                            ~ fix_names_up_to_sfs(look_up_ref = .x,
                                                                  one_cluster_up_to_xmin_list = one_cluster_up_to_xmin_list)) %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_up_to_xmin_list  %>%
                                 base::length())))
  ##
  one_cluster_joint_travel_time_list <- purrr::map(1:(length(one_cluster_unioned_time_bands_list)-1),
                                                   ~ sf::st_difference(one_cluster_unioned_time_bands_list %>% purrr::pluck(.x+1),
                                                                       one_cluster_up_to_xmin_list %>% purrr::pluck(.x)) %>%
                                                     dplyr::select(id,min,max,center,drive_times)) %>%
    stats::setNames(paste0("tb_"
                           ,2:(one_cluster_up_to_xmin_list  %>%
                                 base::length())))  %>%
    purrr::prepend(list(tb_1 = one_cluster_unioned_time_bands_list %>% purrr::pluck(1)))
  return(one_cluster_joint_travel_time_list)
}
