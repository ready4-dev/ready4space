#' @title Create isochrones around an origin using the Travel Time App
#' @description FUNCTION_DESCRIPTION
#' @details ## Implements: https://stackoverflow.com/questions/40489162/draw-time-radius-around-lat-long-on-map
#' @param appID PARAM_DESCRIPTION
#' @param apiKey PARAM_DESCRIPTION
#' @param origin PARAM_DESCRIPTION
#' @param mode_of_transport PARAM_DESCRIPTION, Default: 'driving'
#' @param travel_time_hours PARAM_DESCRIPTION
#' @param crs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[httr]{POST}},\code{\link[httr]{add_headers}}
#'  \code{\link[jsonlite]{toJSON, fromJSON}}
#'  \code{\link[googleway]{encode_pl}}
#'  \code{\link[dplyr]{pull}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}}
#' @rdname travel_time_app_calc
#' @export
#' @importFrom httr POST add_headers
#' @importFrom jsonlite fromJSON
#' @importFrom googleway encode_pl
#' @importFrom dplyr pull
#' @importFrom purrr map reduce
travel_time_app_calc <- function(appID,
                                 apiKey,
                                 origin,
                                 mode_of_transport = "driving",
                                 travel_time_hours,
                                 crs){
  location <- origin
  travel_time_secs <- travel_time_hours * 60 * 60
  url <- "http://api.traveltimeapp.com/v4/time-map"
  requestBody <- paste0('{
                        "departure_searches" : [
                        {"id" : "test",
                        "coords": {"lat":', origin[1], ', "lng":', origin[2],' },
                        "transportation" : {"type" : "', mode_of_transport,'" } ,
                        "travel_time" : ', travel_time_secs, ',
                        "departure_time" : "2017-05-03T08:00:00z"
                        }
                        ]
}')
  res <- httr::POST(url = url,
                    httr::add_headers('Content-Type' = 'application/json'),
                    httr::add_headers('Accept' = 'application/json'),
                    httr::add_headers('X-Application-Id' = appId),
                    httr::add_headers('X-Api-Key' = apiKey),
                    body = requestBody,
                    encode = "json")

  res <- jsonlite::fromJSON(as.character(res))
  pl <- lapply(res$results$shapes[[1]]$shell, function(x){
    googleway::encode_pl(lat = x[['lat']], lon = x[['lng']])
  })
  df <- data.frame(polyline = unlist(pl))
  #df_marker <- data.frame(lat = location[1], lon = location[2])
  polyline_vec <- df %>% dplyr::pull(polyline)
  list_of_sfs <- purrr::map(polyline_vec,
                            ~ convert_tt_polygon_to_sf(tt_polyline = .x,
                                                       mode_of_transport = mode_of_transport,
                                                       travel_time_hours = travel_time_hours,
                                                       crs = crs))

  new_sf <- purrr::reduce(list_of_sfs,
                          rbind)
  return(mew_sf)
}

#' @title convert_tt_polygon_to_sf
#' @description FUNCTION_DESCRIPTION
#' @param tt_polyline PARAM_DESCRIPTION
#' @param mode_of_transport PARAM_DESCRIPTION
#' @param travel_time_hours PARAM_DESCRIPTION
#' @param crs PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[googlePolylines]{decode}}
#'  \code{\link[purrr]{pluck}}
#'  \code{\link[sf]{st}},\code{\link[sf]{sfc}},\code{\link[sf]{sf}}
#' @rdname convert_tt_polygon_to_sf
#' @export
#' @importFrom googlePolylines decode
#' @importFrom purrr pluck
#' @importFrom sf st_polygon st_sfc st_sf
convert_tt_polygon_to_sf <- function(tt_polyline,
                                     mode_of_transport,
                                     travel_time_hours,
                                     crs){
  test_g_polyline <- tt_polyline %>% as.character()
  googlePolylines::decode(test_g_polyline) %>%
    purrr::pluck(1) %>%
    as.matrix() %>%
    list() %>%
    sf::st_polygon() %>%
    sf::st_sfc() %>%
    data.frame(mode_of_transport = mode_of_transport,
               travel_time_hours = travel_time_hours,
               .) %>%
    sf::st_sf(crs = crs)
}
