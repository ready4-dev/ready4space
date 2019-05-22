## Implements: https://rpubs.com/maiae/drivetime
#' @title spatial_area_within_xmin_drive_of_points
#' @description Calculate area within specified drives timeof a cluster of points.
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

#' @title one_cluster_one_service_travel_time
#' @description FUNCTION_DESCRIPTION
#' @param cluster_tb PARAM_DESCRIPTION
#' @param service PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{pull}}
#' @rdname one_cluster_one_service_travel_time
#' @export
#' @importFrom dplyr filter select pull
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

#' @describeIn one_service_time_bands
#' @param look_up_ref PARAM_DESCRIPTION
#' @param one_cluster_travel_time_sf_list PARAM_DESCRIPTION
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param look_up_ref PARAM_DESCRIPTION
#' @param one_cluster_travel_time_sf_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{pluck}},\code{\link[purrr]{map}}
#'  \code{\link[dplyr]{pull}},\code{\link[dplyr]{filter}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[stringr]{str_replace}}
#' @rdname one_service_time_bands
#' @export
#' @importFrom purrr pluck map
#' @importFrom dplyr pull filter
#' @importFrom stats setNames
#' @importFrom stringr str_replace_all
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

#' @title union_one_travel_time_band_across_sites
#' @description FUNCTION_DESCRIPTION
#' @param time_band_ref PARAM_DESCRIPTION
#' @param one_cluster_time_bands_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{map}},\code{\link[purrr]{pluck}},\code{\link[purrr]{reduce}}
#'  \code{\link[sf]{geos_combine}}
#'  \code{\link[dplyr]{select}}
#' @rdname union_one_travel_time_band_across_sites
#' @export
#' @importFrom purrr map pluck reduce
#' @importFrom sf st_union
#' @importFrom dplyr select
union_one_travel_time_band_across_sites <- function(time_band_ref,
                                                    one_cluster_time_bands_list){
  list_of_new_sfs <- purrr::map(one_cluster_time_bands_list,
                                ~ .x %>% purrr::pluck(time_band_ref))
  purrr::reduce(list_of_new_sfs,
                ~ sf::st_union(.x,.y)  %>%
                  dplyr::select(id,min,max,center,drive_times))
}

#' @title fix_names_up_to_sfs
#' @description FUNCTION_DESCRIPTION
#' @param look_up_ref PARAM_DESCRIPTION
#' @param one_cluster_up_to_xmin_list PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{pluck}}
#'  \code{\link[stringr]{str_subset}},\code{\link[stringr]{str_replace}}
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}}
#'  \code{\link[rlang]{sym}}
#' @rdname fix_names_up_to_sfs
#' @export
#' @importFrom purrr pluck
#' @importFrom stringr str_subset str_replace_all
#' @importFrom dplyr mutate select
#' @importFrom rlang sym
fix_names_up_to_sfs <- function(look_up_ref,
                                one_cluster_up_to_xmin_list){
  max_var <- "max"
  max_vec <- one_cluster_up_to_xmin_list %>%
    purrr::pluck(look_up_ref) %>%
    names() %>%
    stringr::str_subset("max")
  if(length(max_vec) > 1){
    max_var <- max_vec %>%
      stringr::str_subset("max.") %>%
      stringr::str_replace_all("max.","") %>%
      as.numeric() %>%
      max() %>%
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

#' @title cluster_isochrones
#' @description FUNCTION_DESCRIPTION
#' @param cluster_tbs_list PARAM_DESCRIPTION
#' @param look_up_ref PARAM_DESCRIPTION
#' @param time_min PARAM_DESCRIPTION, Default: 0
#' @param time_max PARAM_DESCRIPTION, Default: 60
#' @param nbr_time_steps PARAM_DESCRIPTION, Default: 5
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[purrr]{pluck}},\code{\link[purrr]{map}},\code{\link[purrr]{accumulate}},\code{\link[purrr]{prepend}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{pull}}
#'  \code{\link[stats]{setNames}}
#'  \code{\link[sf]{geos_combine}},\code{\link[sf]{geos_binary_ops}}
#' @rdname cluster_isochrones
#' @export
#' @importFrom purrr pluck map accumulate prepend
#' @importFrom dplyr select pull
#' @importFrom stats setNames
#' @importFrom sf st_union st_difference
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
    stats::setNames(one_cluster_travel_time_sf_list %>% names())
  ##
  one_cluster_unioned_time_bands_list <- purrr::map(1:(one_cluster_time_bands_list %>%
                                                         purrr::pluck(1) %>%
                                                         length()),
                                                    ~ union_one_travel_time_band_across_sites(time_band_ref = .x,
                                                                                              one_cluster_time_bands_list =  one_cluster_time_bands_list)) %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_time_bands_list %>%
                                 purrr::pluck(1) %>%
                                 length())))
  ##
  one_cluster_up_to_xmin_list <- purrr::accumulate(one_cluster_unioned_time_bands_list,
                                                   ~ sf::st_union(.x,.y))  %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_unioned_time_bands_list %>%
                                 length())))
  ##
  one_cluster_up_to_xmin_list <- purrr::map(1:length(one_cluster_up_to_xmin_list),
                                            ~ fix_names_up_to_sfs(look_up_ref = .x,
                                                                  one_cluster_up_to_xmin_list = one_cluster_up_to_xmin_list)) %>%
    stats::setNames(paste0("tb_"
                           ,1:(one_cluster_up_to_xmin_list  %>%
                                 length())))
  ##
  one_cluster_joint_travel_time_list <- purrr::map(1:(length(one_cluster_unioned_time_bands_list)-1),
                                                   ~ sf::st_difference(one_cluster_unioned_time_bands_list %>% purrr::pluck(.x+1),
                                                                       one_cluster_up_to_xmin_list %>% purrr::pluck(.x)) %>%
                                                     dplyr::select(id,min,max,center,drive_times)) %>%
    stats::setNames(paste0("tb_"
                           ,2:(one_cluster_up_to_xmin_list  %>%
                                 length())))  %>%
    purrr::prepend(list(tb_1 = one_cluster_unioned_time_bands_list %>% purrr::pluck(1)))
  return(one_cluster_joint_travel_time_list)
}
