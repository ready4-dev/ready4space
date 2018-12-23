#' @title Create a simple features object based on suburbs from which cluster clients come from.
#' @description FUNCTION_DESCRIPTION
#' @param aus_state_suburbs_sf PARAM_DESCRIPTION, Default: ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
#'    lookup_reference = "aus_state_suburbs_sf")
#' @param client_locations_ref PARAM_DESCRIPTION, Default: NULL
#' @param year PARAM_DESCRIPTION, Default: 2018
#' @param cluster PARAM_DESCRIPTION
#' @param state_names PARAM_DESCRIPTION
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
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{pull}},\code{\link[dplyr]{rowwise}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{group_by}},\code{\link[dplyr]{summarise}},\code{\link[dplyr]{select}},\code{\link[dplyr]{join}}
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_length}},\code{\link[stringr]{str_detect}},\code{\link[stringr]{str_order}}
#'  \code{\link[purrr]{map}}
#' @rdname suburb_based_effective_catchment
#' @export
#' @importFrom ready.data data_get
#' @importFrom dplyr filter pull rowwise mutate group_by summarise rename inner_join
#' @importFrom stringr str_sub str_length str_detect str_sort
#' @importFrom purrr map
#'
suburb_based_effective_catchment <- function(aus_state_suburbs_sf = ready.data::data_get(data_lookup_tb = aus_spatial_lookup_tb,
                                                                                         lookup_reference = "aus_ssc_nat_shp_bound_2016",
                                                                                         lookup_variable = "name",
                                                                                         target_variable = "source_reference"),
                                          client_locations_ref = NULL,
                                          year = 2018,
                                          cluster,
                                          state_names){
  if(is.null(client_locations_ref)){
    client_locations_ref <- paste0("InputData/ClientLocation/hyepp/",
                                   cluster,
                                   "/hyepp_clients_",
                                   year,
                                   ".xls")
  }
  client_suburbs_one_cluster_one_year <- client_by_suburb_one_year(cluster = cluster,
                                                                   year = year,
                                                                   file_ref = client_locations_ref)
  included_suburbs_one_cluster_one_year <- get_client_suburb_vector(client_suburbs_one_cluster_one_year)
  ## Get rid of / reassign "Unk"
  all_suburbs_included_states <- aus_state_suburbs_sf %>%
    dplyr::filter(STE_NAME16 %in% state_names) %>% dplyr::pull(SSC_NAME16) %>%
    base::as.character()
  not_matched <- base::setdiff(included_suburbs_one_cluster_one_year,
                               all_suburbs_included_states)
  matched_as_raw <- included_suburbs_one_cluster_one_year[!included_suburbs_one_cluster_one_year %in% not_matched ]
  with_state_ext <- add_st_accronym(x = not_matched,
                                    s_t = state_names[1])
  not_matched_with_state_ext <- base::setdiff(with_state_ext, all_suburbs_included_states)
  matched_with_state_ext <- base::setdiff(with_state_ext,not_matched_with_state_ext)
  suffix_length <- 6
  if(state_names[1] %in% c("Northern Territory",
                          "South Australia",
                          "Western Australia"))
    suffix_length <- 5
  not_matched_raw <- not_matched_with_state_ext %>% stringr::str_sub(1,stringr::str_length(.)-suffix_length)
  manual_changes <- purrr::map(not_matched_raw,
                               ~ reconcile_suburb_names(.x)) %>%
    unlist()
  matched_manual_changes <- manual_changes[!stringr::str_detect(manual_changes,
                                                                base::setdiff(manual_changes,
                                                                              aus_state_suburbs_sf %>%
                                                                                dplyr::pull(SSC_NAME16) %>%
                                                                                base::as.character()))]
  updated_included_suburbs <- c(matched_as_raw,
                                matched_with_state_ext,
                                matched_manual_changes) %>%
    stringr::str_sort()
  updated_client_locations <- client_suburbs_one_cluster_one_year  %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Suburb = apply_naming_convention(Suburb,
                                                   updated_included_suburbs,
                                                   state_territory = state_names[1]))
  suburb_summary <- updated_client_locations %>%
    dplyr::group_by(Suburb) %>%
    dplyr::summarise(clients=n())
  unk_state <- add_st_accronym(x="Unk",
                               s_t = state_names[1])
  known_suburb_summary <- suburb_summary %>%
    dplyr::filter(Suburb != unk_state)
  known_suburb_summary <- known_suburb_summary %>%
    dplyr::rename(SSC_NAME16=Suburb)
  effective_catchment_sf <- dplyr::inner_join(aus_state_suburbs_sf,
                                                            known_suburb_summary)
  return(effective_catchment_sf)
}

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param outliers PARAM_DESCRIPTION
#' @param suburb_based_effective_catch_inc_outliers_sf PARAM_DESCRIPTION
suburb_based_effective_catch_excl_outliers <- function(outliers,
                                                       suburb_based_effective_catch_inc_outliers_sf){
  inc_suburbs_inc_outliers <- suburb_based_effective_catch_inc_outliers_sf %>%
    dplyr::pull(SSC_NAME16)
  remove_outlier <- function(x,y) x[!stringr::str_detect(x,y)]
  inc_suburbs_excl_outliers <- as.list(outliers) %>%
    stats::setNames(outliers) %>%
    purrr::prepend(list(a = inc_suburbs_inc_outliers)) %>%
    purrr::reduce(remove_outlier)
  effective_catchment_sf <- suburb_based_effective_catch_inc_outliers_sf %>%
    dplyr::filter(SSC_NAME16 %in%
                    inc_suburbs_excl_outliers) %>%
    sf::st_as_sf()
  return(effective_catchment_sf)
}

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param cluster PARAM_DESCRIPTION
#' @param year PARAM_DESCRIPTION
#' @param file_ref PARAM_DESCRIPTION
client_by_suburb_one_year <- function(cluster,
                                      year,
                                      file_ref = NA){
  if(is.na(file_ref)){
    client_locations <- readxl::read_xls(paste0("InputData/ClientLocation/hyepp/",
                                                cluster,
                                                "/hyepp_clients_",
                                                year,
                                                ".xls"))
  }else
    client_locations <- readxl::read_xls(file_ref)
  client_locations <- client_locations %>% dplyr::filter(!is.na(TeamName))  %>%
    dplyr::filter(TeamName=="hYEPP") %>%
    dplyr::mutate(Suburb = proper(Suburb)) %>%
    dplyr::mutate(Suburb = stringr::str_to_title(Suburb))
  return(client_locations)
}
#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param x PARAM_DESCRIPTION
proper <- function(x) paste0(toupper(substr(x, 1, 1)),
                             tolower(substring(x, 2)))
#add_nsw <- function(x) paste0(x," (NSW)")
#remove_nsw <-function(x) stringr::str_replace(x," (NSW)","")

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param x PARAM_DESCRIPTION
#' @param s_t PARAM_DESCRIPTION
#' @param included_sts PARAM_DESCRIPTION
add_st_accronym <- function(x,
                            s_t,
                            included_sts = c("Australian Capital Territory",
                                             "New South Wales",
                                             "Northern Territory",
                                             "Queensland",
                                             "South Australia",
                                             "Tasmania",
                                             "Victoria",
                                             "Western Australia")) {
  acc_key <- tibble::tibble(name = included_sts,
                            acc = c("ACT", "NSW", "NT", "QLD", "SA","TAS", "VIC", "WA"))
  accronym <- acc_key %>% dplyr::filter(included_sts == !!(s_t)) %>% dplyr::pull(acc)
  paste0(x," (",accronym,")")
}

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param x PARAM_DESCRIPTION
#' @param s_t PARAM_DESCRIPTION
#' @param included_sts PARAM_DESCRIPTION
remove_st_accronym <- function(x,
                               s_t,
                               included_sts = c("Australian Capital Territory",
                                                "New South Wales",
                                                "Northern Territory",
                                                "Queensland",
                                                "South Australia",
                                                "Tasmania",
                                                "Victoria",
                                                "Western Australia")){
  acc_key <- tibble::tibble(name = included_sts,
                            acc = c("ACT", "NSW", "NT", "QLD", "SA","TAS", "VIC", "WA"))
  accronym <- acc_key %>% dplyr::filter(included_sts == !!(s_t)) %>% dplyr::pull(acc)
  stringr::str_replace(x,paste0(" (",accronym,")"),"")
}

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param x PARAM_DESCRIPTION
#' @param names_vector PARAM_DESCRIPTION
apply_naming_convention <- function(x,
                                    names_vector,
                                    state_territory){
  updated_name <- stringr::str_to_title(x)
  updated_name <- reconcile_suburb_names(updated_name)
  if(!updated_name %in% names_vector){
    updated_name <- add_st_accronym(x = updated_name,
                                    s_t = state_territory) #paste0(updated_name," (NSW)")
  }
  return(updated_name)
}

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param raw_name PARAM_DESCRIPTION
reconcile_suburb_names <- function(raw_name){
  ## Convert below to new function with lookup table.
  ## Alternatively, create function to search for "Name East" / "East Name"
  ## All options that contain first then/or second word in string
  ## Pick closest by distance
  new_name <- raw_name %>%
    stringr::str_replace("Kingswood", "Kingswood (Penrith - NSW)") %>%
    stringr::str_replace("Kurrajong East", "East Kurrajong") %>%
    stringr::str_replace("Parramatta North", "North Parramatta") %>%
    stringr::str_replace("Penrith South", "South Penrith") %>%
    stringr::str_replace("Maryland", "Maryland (Newcastle - NSW)") %>%
    stringr::str_replace("Ropes Creek", "Ropes Crossing") %>%
    stringr::str_replace("St Clair", "St Clair (Penrith - NSW)") %>%
    stringr::str_replace("Wood Park", "Woodpark")
  return(new_name)
}

#' @describeIn suburb_based_effective_catchment Calculates the .....
#' @param clients_by_suburb PARAM_DESCRIPTION
get_client_suburb_vector <- function(clients_by_suburb){
  included_suburbs <- clients_by_suburb %>%
    dplyr::select(Suburb) %>%
    base::unique() %>%
    dplyr::pull() %>%
    stringr::str_sort()
  return(included_suburbs)
}



