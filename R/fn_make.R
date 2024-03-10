#' Make agent locations tibble
#' @description make_agent_locations_tb() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make agent locations tibble. The function returns Agent coordinates (a tibble).
#' @param profiled_area_sf Profiled area (a simple features object)
#' @param crs_dbl Coordinates reference system (a double vector)
#' @param person_ctg_1L_chr Person category (a character vector of length one), Default: character(0)
#' @param person_type_1L_chr Person type (a character vector of length one), Default: 'p'
#' @param resolution_unit_1L_chr Resolution unit (a character vector of length one)
#' @param year_1L_chr Year (a character vector of length one)
#' @return Agent coordinates (a tibble)
#' @rdname make_agent_locations_tb
#' @export 
#' @importFrom stringr str_sub
#' @importFrom sf st_set_geometry
#' @importFrom dplyr select pull filter
#' @importFrom rlang sym
#' @importFrom purrr map2_dfr
#' @keywords internal
make_agent_locations_tb <- function (profiled_area_sf, crs_dbl, person_ctg_1L_chr = character(0), 
    person_type_1L_chr = "p", resolution_unit_1L_chr, year_1L_chr) 
{
    unit_col_name_1L_chr <- paste0(resolution_unit_1L_chr, "_MAIN", 
        stringr::str_sub(year_1L_chr, 3, 4))
    cases_col_name_1L_chr <- paste0("proj_", person_ctg_1L_chr, 
        ifelse(identical(person_ctg_1L_chr, character(0)), "", 
            "_"), person_type_1L_chr, "_", year_1L_chr)
    profiled_area_df <- profiled_area_sf %>% sf::st_set_geometry(NULL) %>% 
        dplyr::select(!!rlang::sym(unit_col_name_1L_chr), !!rlang::sym(cases_col_name_1L_chr))
    agent_coordinates_tb <- purrr::map2_dfr(profiled_area_df %>% 
        dplyr::select(!!unit_col_name_1L_chr) %>% dplyr::pull(), 
        profiled_area_df %>% dplyr::select(!!rlang::sym(cases_col_name_1L_chr)) %>% 
            dplyr::pull(), ~randomise_locations(profiled_sf = profiled_area_sf %>% 
            dplyr::filter(!!rlang::sym(unit_col_name_1L_chr) == 
                .x), cases_int = .y, crs_dbl = crs_dbl))
    return(agent_coordinates_tb)
}
#' Make closest years list
#' @description make_closest_yrs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make closest years list. The function returns Closest years (a list).
#' @param data_lookup_tb Data lookup (a tibble)
#' @param main_incld_feature_chr Main included feature (a character vector)
#' @param target_year_1L_chr Target year (a character vector of length one)
#' @param target_area_1L_chr Target area (a character vector of length one), Default: character(0)
#' @param approximation_1L_chr Approximation (a character vector of length one), Default: 'abs'
#' @return Closest years (a list)
#' @rdname make_closest_yrs_ls
#' @export 
#' @importFrom dplyr filter pull
#' @importFrom purrr map
#' @keywords internal
make_closest_yrs_ls <- function (data_lookup_tb, main_incld_feature_chr, target_year_1L_chr, 
    target_area_1L_chr = character(0), approximation_1L_chr = "abs") 
{
    if (!identical(target_area_1L_chr, character(0))) {
        data_lookup_tb <- data_lookup_tb %>% dplyr::filter(area_type_chr == 
            target_area_1L_chr)
    }
    available_yrs_ls <- purrr::map(main_incld_feature_chr, ~data_lookup_tb %>% 
        dplyr::filter(main_feature_chr == .x) %>% dplyr::pull(year_chr) %>% 
        as.numeric())
    if (approximation_1L_chr == "abs") {
        closest_yrs_ls <- purrr::map(available_yrs_ls, ~.x[which(abs(.x - 
            as.numeric(target_year_1L_chr)) == min(abs(.x - as.numeric(target_year_1L_chr))))])
    }
    if (approximation_1L_chr == "previous") {
        closest_yrs_ls <- purrr::map(available_yrs_ls, ~.x[which(as.numeric(target_year_1L_chr) - 
            .x == min(max(as.numeric(target_year_1L_chr) - .x, 
            0)))])
    }
    if (approximation_1L_chr == "next") {
        closest_yrs_ls <- purrr::map(available_yrs_ls, ~.x[which(.x - 
            as.numeric(target_year_1L_chr) == min(max(.x - as.numeric(target_year_1L_chr), 
            0)))])
    }
    return(closest_yrs_ls)
}
#' Make cluster boundaries
#' @description make_cluster_bndys() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cluster boundaries. The function returns Cluster boundaries (a list).
#' @param clusters_chr Clusters (a character vector)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param distance_in_km_1L_dbl Distance in kilometre (a double vector of length one)
#' @param land_boundary_sf Land boundary (a simple features object)
#' @param vicinity_points_ls Vicinity points (a list)
#' @return Cluster boundaries (a list)
#' @rdname make_cluster_bndys
#' @export 
#' @importFrom purrr map pluck
#' @importFrom stats setNames
#' @keywords internal
make_cluster_bndys <- function (clusters_chr, crs_nbr_dbl, distance_in_km_1L_dbl, land_boundary_sf, 
    vicinity_points_ls) 
{
    cluster_bndys_ls <- purrr::map(1:length(clusters_chr), ~manufacture.vicinity_points(vicinity_points_ls %>% 
        purrr::pluck(.x), land_sf = land_boundary_sf, metres_1L_dbl = distance_in_km_1L_dbl * 
        1000, crs_nbr_dbl = crs_nbr_dbl, what_1L_chr == "geometric")) %>% 
        stats::setNames(., vicinity_points_ls %>% names())
    return(cluster_bndys_ls)
}
#' Make cluster isochrones
#' @description make_cluster_isochrones() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make cluster isochrones. The function returns Discrete temporal bands (a list).
#' @param vicinity_points_ls Vicinity points (a list)
#' @param index_val_1L_int Index value (an integer vector of length one)
#' @param time_min_1L_dbl Time minimum (a double vector of length one), Default: 0
#' @param time_max_1L_dbl Time maximum (a double vector of length one), Default: 30
#' @param time_steps_1L_dbl Time steps (a double vector of length one), Default: 5
#' @param travel_mode_1L_chr Travel mode (a character vector of length one), Default: 'car'
#' @return Discrete temporal bands (a list)
#' @rdname make_cluster_isochrones
#' @export 
#' @importFrom purrr pluck map accumulate
#' @importFrom dplyr select pull mutate
#' @importFrom stats setNames
#' @importFrom sf st_union st_difference
#' @importFrom rlang sym
#' @keywords internal
make_cluster_isochrones <- function (vicinity_points_ls, index_val_1L_int, time_min_1L_dbl = 0, 
    time_max_1L_dbl = 30, time_steps_1L_dbl = 5, travel_mode_1L_chr = "car") 
{
    cluster_services_chr <- vicinity_points_ls %>% purrr::pluck(index_val_1L_int) %>% 
        dplyr::select(service_name_chr) %>% dplyr::pull()
    x_vicinity_points <- vicinity_points_ls %>% purrr::pluck(index_val_1L_int)
    cluster_isochrone_ls <- purrr::map(cluster_services_chr, 
        ~manufacture.vicinity_points(x_vicinity_points, service_1L_chr = .x, 
            time_min_1L_dbl = time_min_1L_dbl, time_max_1L_dbl = time_max_1L_dbl, 
            time_steps_1L_dbl = time_steps_1L_dbl, travel_mode_1L_chr = travel_mode_1L_chr, 
            what_1L_chr = "isochrones")) %>% stats::setNames(., 
        cluster_services_chr)
    isochrone_bands_ls <- purrr::map(1:length(cluster_isochrone_ls), 
        ~make_isochrone_bands(index_val_1L_int = .x, cluster_isochrone_ls = cluster_isochrone_ls, 
            travel_mode_1L_chr = travel_mode_1L_chr)) %>% stats::setNames(cluster_isochrone_ls %>% 
        names())
    unioned_isochrones_ls <- purrr::map(1:(isochrone_bands_ls %>% 
        purrr::pluck(1) %>% length()), ~bind_isochrone_bands(isochrone_bands_ls = isochrone_bands_ls, 
        index_1L_int = .x, travel_mode_1L_chr = travel_mode_1L_chr)) %>% 
        stats::setNames(paste0("tb_", 1:(isochrone_bands_ls %>% 
            purrr::pluck(1) %>% length())))
    temporal_bands_ls <- purrr::accumulate(2:length(unioned_isochrones_ls), 
        .init = unioned_isochrones_ls[[1]], .simplify = F, ~sf::st_union(.x, 
            unioned_isochrones_ls[[.y]])) %>% stats::setNames(paste0("tb_", 
        1:(unioned_isochrones_ls %>% length())))
    temporal_bands_ls <- purrr::map(1:length(temporal_bands_ls), 
        ~update_isochrone_tbl(index_val_1L_int = .x, temporal_bands_ls = temporal_bands_ls, 
            travel_mode_1L_chr = travel_mode_1L_chr)) %>% stats::setNames(paste0("tb_", 
        1:(temporal_bands_ls %>% length())))
    discrete_temporal_bands_ls <- purrr::map(1:(length(unioned_isochrones_ls) - 
        1), ~sf::st_difference(unioned_isochrones_ls %>% purrr::pluck(.x + 
        1), temporal_bands_ls %>% purrr::pluck(.x)) %>% dplyr::select(id, 
        isomin, isomax, center_value, !!rlang::sym(paste0(travel_mode_1L_chr, 
            "_times")))) %>% stats::setNames(paste0("tb_", 2:(temporal_bands_ls %>% 
        length()))) %>% append(list(tb_1 = unioned_isochrones_ls %>% 
        purrr::pluck(1) %>% dplyr::mutate(center_value = (isomin + 
        isomax)/2) %>% dplyr::select(id, isomin, isomax, center_value, 
        !!rlang::sym(paste0(travel_mode_1L_chr, "_times")))), 
        after = 0)
    return(discrete_temporal_bands_ls)
}
#' Make common simple features object variables list
#' @description make_common_sf_vars_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make common simple features object variables list. The function returns Common simple features object variables (a list).
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return Common simple features object variables (a list)
#' @rdname make_common_sf_vars_ls
#' @export 
#' @importFrom purrr map
#' @keywords internal
make_common_sf_vars_ls <- function (sf_ls) 
{
    vec_ls <- purrr::map(sf_ls, ~names(.x))
    common_sf_vars_ls <- Reduce(intersect, vec_ls)
    return(common_sf_vars_ls)
}
#' Make common simple features object years list
#' @description make_common_sf_yrs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make common simple features object years list. The function returns Common simple features object years (a list).
#' @param sf_ls Simple features object list (a list of simple features objects)
#' @return Common simple features object years (a list)
#' @rdname make_common_sf_yrs_ls
#' @export 
#' @importFrom purrr map
#' @keywords internal
make_common_sf_yrs_ls <- function (sf_ls) 
{
    vec_ls <- purrr::map(list_of_sfs, ~get_included_yrs(.x))
    common_sf_yrs_ls <- Reduce(intersect, vec_ls)
    return(common_sf_yrs_ls)
}
#' Make data years character vector
#' @description make_data_yrs_chr() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make data years character vector. The function returns Data years (a character vector).
#' @param data_dtm Data (a date vector)
#' @return Data years (a character vector)
#' @rdname make_data_yrs_chr
#' @export 
#' @importFrom lubridate year
#' @keywords internal
make_data_yrs_chr <- function (data_dtm) 
{
    data_yrs_chr <- data_dtm %>% lubridate::year() %>% as.character()
    return(data_yrs_chr)
}
#' Make featured variable prefix
#' @description make_featured_var_pfx() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make featured variable prefix. The function returns Prefix (a character vector of length one).
#' @param dynamic_var_rsl_1L_chr Dynamic variable resolution (a character vector of length one)
#' @param reference_vals_chr Reference values (a character vector)
#' @param reference_var_rsl_1L_chr Reference variable resolution (a character vector of length one), Default: NULL
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @return Prefix (a character vector of length one)
#' @rdname make_featured_var_pfx
#' @export 
#' @keywords internal
make_featured_var_pfx <- function (dynamic_var_rsl_1L_chr, reference_vals_chr, reference_var_rsl_1L_chr = NULL, 
    data_year_1L_chr) 
{
    if (!is.null(reference_var_rsl_1L_chr)) {
        nse_names_ls <- make_nse_objs_ls(spatial_unit_1L_chr = reference_var_rsl_1L_chr, 
            concept_1L_chr = reference_vals_chr[1], reference_var_nm_1L_chr = paste0("year_", 
                data_year_1L_chr, "pr"), grouping_var_1L_chr = dynamic_var_rsl_1L_chr, 
            data_year_1L_chr = data_year_1L_chr)
    }
    else {
        nse_names_ls <- make_nse_objs_ls(spatial_unit_1L_chr = dynamic_var_rsl_1L_chr, 
            concept_1L_chr = reference_vals_chr[2], grouping_var_1L_chr = dynamic_var_rsl_1L_chr, 
            data_year_1L_chr = data_year_1L_chr)
    }
    prefix_1L_chr <- paste0(nse_names_ls$popl_inc_unit, "_")
    return(prefix_1L_chr)
}
#' Make filter by year logic
#' @description make_filter_by_year_logic() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make filter by year logic. The function returns Filter by year (a logical vector).
#' @param data_tb Data (a tibble)
#' @param years_chr Years (a character vector)
#' @return Filter by year (a logical vector)
#' @rdname make_filter_by_year_logic
#' @export 
#' @importFrom purrr map2_lgl
#' @keywords internal
make_filter_by_year_logic <- function (data_tb, years_chr) 
{
    filter_by_year_lgl <- purrr::map2_lgl(data_tb$year_chr, data_tb$year_start_chr, 
        ~(.x %in% years_chr | .y %in% years_chr))
    return(filter_by_year_lgl)
}
#' Make intersecting geometries
#' @description make_intersecting_geometries() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make intersecting geometries. The function returns Intersection (a simple features object).
#' @param geometry_one_sf Geometry one (a simple features object)
#' @param geometry_two_sf Geometry two (a simple features object)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param validate_1L_lgl Validate (a logical vector of length one), Default: T
#' @return Intersection (a simple features object)
#' @rdname make_intersecting_geometries
#' @export 
#' @importFrom sf st_intersection st_transform
#' @keywords internal
make_intersecting_geometries <- function (geometry_one_sf, geometry_two_sf, crs_nbr_dbl, validate_1L_lgl = T) 
{
    intersection_sf <- sf::st_intersection(geometry_one_sf %>% 
        sf::st_transform(crs_nbr_dbl[2]), geometry_two_sf %>% 
        sf::st_transform(crs_nbr_dbl[2])) %>% sf::st_transform(crs_nbr_dbl[1])
    if (validate_1L_lgl) 
        intersection_sf <- intersection_sf %>% make_valid_new_sf()
    return(intersection_sf)
}
#' Make intersecting profiled area
#' @description make_intersecting_profiled_area() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make intersecting profiled area. The function returns Profiled (a simple features object).
#' @param attribute_rsl_1L_chr Attribute resolution (a character vector of length one)
#' @param attribute_sf Attribute (a simple features object)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param data_type_chr Data type (a character vector)
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param profiled_sf Profiled (a simple features object)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one), Default: character(0)
#' @param profiled_sf_col_1L_chr Profiled simple features object column (a character vector of length one), Default: 'NA'
#' @param profiled_sf_row_1L_chr Profiled simple features object row (a character vector of length one), Default: 'NA'
#' @return Profiled (a simple features object)
#' @rdname make_intersecting_profiled_area
#' @export 
#' @importFrom dplyr select pull slice
#' @importFrom stringr str_which
#' @keywords internal
make_intersecting_profiled_area <- function (attribute_rsl_1L_chr, attribute_sf, crs_nbr_dbl, data_type_chr, 
    data_year_1L_chr, profiled_sf, featured_var_pfx_1L_chr = character(0), 
    profiled_sf_col_1L_chr = NA_character_, profiled_sf_row_1L_chr = NA_character_) 
{
    if (!is.na(profiled_sf_col_1L_chr)) {
        if (!is.na(profiled_sf_row_1L_chr)) {
            profiled_sf <- profiled_sf %>% dplyr::select(!!profiled_sf_col_1L_chr)
            index.nbr <- profiled_sf %>% dplyr::pull(profiled_sf_col_1L_chr) %>% 
                stringr::str_which(profiled_sf_row_1L_chr)
            profiled_sf <- profiled_sf %>% dplyr::slice(index.nbr)
        }
        else profiled_sf <- profiled_sf %>% dplyr::select(!!profiled_sf_col_1L_chr)
    }
    attribute_sf <- rename_vars_based_on_res(sf = attribute_sf, 
        data_type_chr = data_type_chr, data_year_1L_chr = data_year_1L_chr, 
        featured_var_pfx_1L_chr = featured_var_pfx_1L_chr, feature_nm_1L_chr = attribute_rsl_1L_chr)
    profiled_sf <- make_intersecting_geometries(geometry_one_sf = profiled_sf, 
        geometry_two_sf = attribute_sf, crs_nbr_dbl = crs_nbr_dbl)
    return(profiled_sf)
}
#' Make isochrone bands
#' @description make_isochrone_bands() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make isochrone bands. The function returns Time band (a list of simple features objects).
#' @param index_val_1L_int Index value (an integer vector of length one)
#' @param cluster_isochrone_ls Cluster isochrone (a list)
#' @param travel_mode_1L_chr Travel mode (a character vector of length one)
#' @return Time band (a list of simple features objects)
#' @rdname make_isochrone_bands
#' @export 
#' @importFrom purrr pluck map
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @importFrom stringr str_replace_all
#' @keywords internal
make_isochrone_bands <- function (index_val_1L_int, cluster_isochrone_ls, travel_mode_1L_chr) 
{
    travel_time_bands <- cluster_isochrone_ls %>% purrr::pluck(index_val_1L_int) %>% 
        dplyr::pull(!!rlang::sym(paste0(travel_mode_1L_chr, "_times")))
    time_band_sf_ls <- purrr::map(travel_time_bands, ~cluster_isochrone_ls %>% 
        purrr::pluck(index_val_1L_int) %>% dplyr::filter(!!rlang::sym(paste0(travel_mode_1L_chr, 
        "_times")) == .x)) %>% stats::setNames(paste0("tb_", 
        stringr::str_replace_all(travel_time_bands, " ", "_")))
    return(time_band_sf_ls)
}
#' Make isochrones
#' @description make_isochrones() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make isochrones. The function returns Iso (a simple features object).
#' @param lat_1L_dbl Lat (a double vector of length one)
#' @param lng_1L_dbl Longitude (a double vector of length one)
#' @param server_1L_chr Server (a character vector of length one), Default: character(0)
#' @param time_min_1L_dbl Time minimum (a double vector of length one)
#' @param time_max_1L_dbl Time maximum (a double vector of length one)
#' @param time_steps_1L_dbl Time steps (a double vector of length one)
#' @param travel_mode_1L_chr Travel mode (a character vector of length one), Default: 'car'
#' @return Iso (a simple features object)
#' @rdname make_isochrones
#' @export 
#' @importFrom osrm osrmIsochrone
#' @importFrom sf st_as_sf
#' @importFrom dplyr mutate arrange
#' @importFrom rlang sym
#' @keywords internal
make_isochrones <- function (lat_1L_dbl, lng_1L_dbl, server_1L_chr = character(0), 
    time_min_1L_dbl, time_max_1L_dbl, time_steps_1L_dbl, travel_mode_1L_chr = "car") 
{
    if (identical(server_1L_chr, character(0))) 
        server_1L_chr <- getOption("osrm.server")
    if (identical(travel_mode_1L_chr, character(0))) 
        travel_mode_1L_chr <- getOption("osrm.profile")
    step_1L_dbl <- (time_max_1L_dbl - time_min_1L_dbl)/time_steps_1L_dbl
    iso_sf <- osrm::osrmIsochrone(loc = c(lng_1L_dbl, lat_1L_dbl), 
        breaks = seq(from = time_min_1L_dbl, to = time_max_1L_dbl, 
            by = step_1L_dbl), osrm.profile = travel_mode_1L_chr, 
        osrm.server = server_1L_chr)
    iso_sf <- sf::st_as_sf(iso_sf) %>% dplyr::mutate(`:=`(!!rlang::sym(paste0(travel_mode_1L_chr, 
        "_times")), paste0(isomin, " to ", isomax, " mins"))) %>% 
        dplyr::arrange(id)
    return(iso_sf)
}
#' Make kilometre squared double vector
#' @description make_km_sqd_dbl() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make kilometre squared double vector. The function returns Kilometre squared (a double vector).
#' @param data_sf Data (a simple features object)
#' @return Kilometre squared (a double vector)
#' @rdname make_km_sqd_dbl
#' @export 
#' @importFrom dplyr mutate summarise pull
#' @importFrom sf st_area
#' @importFrom units set_units
#' @keywords internal
make_km_sqd_dbl <- function (data_sf) 
{
    km_sqd_dbl <- data_sf %>% dplyr::mutate(FT_AREA_SQKM = sf::st_area(.) %>% 
        units::set_units(km^2)) %>% dplyr::summarise(TOT_AREA_SQKM = sum(FT_AREA_SQKM)) %>% 
        dplyr::pull(TOT_AREA_SQKM)
    return(km_sqd_dbl)
}
#' Make non-standard evaluation objects list
#' @description make_nse_objs_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make non-standard evaluation objects list. The function returns Non-standard evaluation objects (a list).
#' @param concept_1L_chr Concept (a character vector of length one)
#' @param spatial_unit_1L_chr Spatial unit (a character vector of length one)
#' @param reference_var_nm_1L_chr Reference variable name (a character vector of length one), Default: NULL
#' @param grouping_var_1L_chr Grouping variable (a character vector of length one), Default: NULL
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param featured_var_pfx_1L_chr Featured variable prefix (a character vector of length one)
#' @return Non-standard evaluation objects (a list)
#' @rdname make_nse_objs_ls
#' @export 
#' @keywords internal
make_nse_objs_ls <- function (concept_1L_chr, spatial_unit_1L_chr, reference_var_nm_1L_chr = NULL, 
    grouping_var_1L_chr = NULL, data_year_1L_chr, featured_var_pfx_1L_chr) 
{
    if (concept_1L_chr == "age_sex") {
        popl_multiplier <- paste0("inc_", spatial_unit_1L_chr, 
            "_prop")
        whl_pop_str_1 <- paste0("whl_", spatial_unit_1L_chr, 
            "_", featured_var_pfx_1L_chr, "y", data_year_1L_chr, 
            ".Females.")
        whl_pop_str_2 <- paste0("whl_", spatial_unit_1L_chr, 
            "_", featured_var_pfx_1L_chr, "y", data_year_1L_chr, 
            ".Males.")
        inc_str_to_delete <- paste0("whl_", spatial_unit_1L_chr, 
            "_")
        grouping_age_sex_popl_1L_chr <- NA_character_
    }
    if (concept_1L_chr == "tot_pop") {
        popl_multiplier <- "pop_prop_multiplier_tot_pop"
        grouping_age_sex_popl_1L_chr <- paste0("grp_by_", grouping_var_1L_chr, 
            "_inc_age_sex_")
        whl_pop_str_1 <- paste0(grouping_age_sex_popl_1L_chr, 
            "y", data_year_1L_chr, ".Females.")
        whl_pop_str_2 <- paste0(grouping_age_sex_popl_1L_chr, 
            "y", data_year_1L_chr, ".Males.")
        inc_str_to_delete <- grouping_age_sex_popl_1L_chr
        grouping_age_sex_popl_1L_chr <- paste0("grp_by_", grouping_var_1L_chr, 
            "_inc_age_sex_")
    }
    nse_objs_ls <- list(area_whl_unit = paste0("whl_", spatial_unit_1L_chr, 
        "_area"), area_inc_unit = paste0("inc_", spatial_unit_1L_chr, 
        "_area"), prop_inc_unit = paste0("inc_", spatial_unit_1L_chr, 
        "_prop"), popl_inc_unit = paste0("inc_", spatial_unit_1L_chr, 
        "_popl"), popl_whl_unit = paste0("whl_", spatial_unit_1L_chr, 
        "_", reference_var_nm_1L_chr), popl_multiplier = popl_multiplier, 
        popl_whl_starts_with_1 = ifelse(is.null(whl_pop_str_1), 
            NA_character_, whl_pop_str_1), popl_whl_starts_with_2 = ifelse(is.null(whl_pop_str_2), 
            NA_character_, whl_pop_str_2), grouping_1_concept_tot = ifelse(is.null(grouping_var_1L_chr), 
            NA_character_, paste0("grp_by_", grouping_var_1L_chr, 
                "_inc_", concept_1L_chr)), grouping_1_age_sex_pop = grouping_age_sex_popl_1L_chr, 
        inc_str_to_delete = inc_str_to_delete)
    return(nse_objs_ls)
}
#' Make path for raw output directory
#' @description make_path_for_raw_outp_dir() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make path for raw output directory. The function is called for its side effects and does not return a value.
#' @param category_1L_chr Category (a character vector of length one)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one)
#' @return No return value, called for side effects.
#' @rdname make_path_for_raw_outp_dir
#' @export 
#' @keywords internal
make_path_for_raw_outp_dir <- function (category_1L_chr, raw_fls_dir_1L_chr) 
{
    paste0(raw_fls_dir_1L_chr, "/", category_1L_chr)
}
#' Make paths to files for ingest
#' @description make_paths_to_fls_for_ingest() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make paths to files for ingest. The function returns Paths (a character vector).
#' @param data_type_chr Data type (a character vector)
#' @param name_chr Name (a character vector)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one)
#' @return Paths (a character vector)
#' @rdname make_paths_to_fls_for_ingest
#' @export 
#' @keywords internal
make_paths_to_fls_for_ingest <- function (data_type_chr, name_chr, processed_fls_dir_1L_chr) 
{
    if (data_type_chr == "Geometry") 
        name_chr <- paste0(name_chr, "_sf")
    paths_chr <- paste0(processed_fls_dir_1L_chr, "/", name_chr, 
        ".RDS")
    return(paths_chr)
}
#' Make polygons from duplicates
#' @description make_polygons_from_duplicates() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make polygons from duplicates. The function returns Polygons (a simple features object).
#' @param sf Simple features object (a simple features object)
#' @param uid_1L_chr Unique identifier (a character vector of length one)
#' @return Polygons (a simple features object)
#' @rdname make_polygons_from_duplicates
#' @export 
#' @importFrom dplyr filter pull summarise_all first
#' @importFrom sf st_is_valid st_sf st_set_geometry st_union st_sfc
#' @importFrom rlang sym
#' @importFrom purrr map reduce
#' @keywords internal
make_polygons_from_duplicates <- function (sf, uid_1L_chr) 
{
    sf <- sf %>% dplyr::filter(sf::st_is_valid(sf))
    duplicates_chr <- sf %>% dplyr::filter(!!rlang::sym(uid_1L_chr) %>% 
        duplicated()) %>% dplyr::pull(!!rlang::sym(uid_1L_chr)) %>% 
        unique()
    geometry_one_sf <- sf %>% dplyr::filter(!(!!rlang::sym(uid_1L_chr) %in% 
        duplicates_chr))
    geometry_two_sf <- sf %>% dplyr::filter(!!rlang::sym(uid_1L_chr) %in% 
        duplicates_chr)
    polygons_sf <- purrr::map(duplicates_chr, ~sf::st_sf(geometry_two_sf %>% 
        dplyr::filter(!!rlang::sym(uid_1L_chr) == .x) %>% sf::st_set_geometry(NULL) %>% 
        dplyr::summarise_all(.funs = dplyr::first), geometry = geometry_two_sf %>% 
        dplyr::filter(!!rlang::sym(uid_1L_chr) == .x) %>% sf::st_union() %>% 
        sf::st_sfc())) %>% append(list(geometry_one_sf)) %>% 
        purrr::reduce(~rbind(.x, .y))
    return(polygons_sf)
}
#' Make reconciled intersecting area
#' @description make_reconciled_intersecting_area() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make reconciled intersecting area. The function returns Profiled (a simple features object).
#' @param profiled_sf Profiled (a simple features object)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param data_year_1L_chr Data year (a character vector of length one)
#' @param dynamic_var_rsl_1L_chr Dynamic variable resolution (a character vector of length one)
#' @param group_by_var_1L_chr Group by variable (a character vector of length one)
#' @param reference_grouping_1L_chr Reference grouping (a character vector of length one)
#' @param reference_vals_chr Reference values (a character vector)
#' @param reference_var_rsl_1L_chr Reference variable resolution (a character vector of length one)
#' @param spatial_atts_ls Spatial attributes (a list)
#' @param profiled_sf_col_1L_chr Profiled simple features object column (a character vector of length one), Default: NA
#' @param profiled_sf_row_1L_chr Profiled simple features object row (a character vector of length one), Default: NA
#' @return Profiled (a simple features object)
#' @rdname make_reconciled_intersecting_area
#' @export 
#' @importFrom sf st_set_geometry
#' @importFrom dplyr distinct select ends_with rename_at vars
#' @importFrom stringi stri_replace_last_regex
#' @keywords internal
make_reconciled_intersecting_area <- function (profiled_sf, crs_nbr_dbl, data_year_1L_chr, dynamic_var_rsl_1L_chr, 
    group_by_var_1L_chr, reference_grouping_1L_chr, reference_vals_chr, 
    reference_var_rsl_1L_chr, spatial_atts_ls, profiled_sf_col_1L_chr = NA, 
    profiled_sf_row_1L_chr = NA) 
{
    if (!is.null(reference_var_rsl_1L_chr)) {
        if (reference_grouping_1L_chr %in% names(spatial_atts_ls[[reference_var_rsl_1L_chr]])) {
            spatial_atts_ls[[dynamic_var_rsl_1L_chr]] <- merge(spatial_atts_ls[[reference_var_rsl_1L_chr]], 
                sf::st_set_geometry(spatial_atts_ls[[dynamic_var_rsl_1L_chr]], 
                  NULL), by = reference_grouping_1L_chr) %>% 
                dplyr::distinct(.keep_all = T) %>% dplyr::select(-dplyr::ends_with(".x")) %>% 
                dplyr::rename_at(.vars = dplyr::vars(dplyr::ends_with(".y")), 
                  ~stringi::stri_replace_last_regex(.x, "\\.y$", 
                    ""))
            spatial_atts_ls[[dynamic_var_rsl_1L_chr]] <- rename_vars_based_on_res(sf = spatial_atts_ls[[dynamic_var_rsl_1L_chr]], 
                data_type_chr = reference_vals_chr[1], data_year_1L_chr = data_year_1L_chr, 
                feature_nm_1L_chr = reference_var_rsl_1L_chr) %>% 
                add_km_sqd(feature_nm_1L_chr = reference_var_rsl_1L_chr)
        }
    }
    spatial_atts_ls[[dynamic_var_rsl_1L_chr]] <- spatial_atts_ls[[dynamic_var_rsl_1L_chr]] %>% 
        add_km_sqd_by_group(group_by_var_1L_chr = reference_grouping_1L_chr, 
            feature_nm_1L_chr = dynamic_var_rsl_1L_chr)
    profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf, 
        profiled_sf_col_1L_chr = profiled_sf_col_1L_chr, profiled_sf_row_1L_chr = profiled_sf_row_1L_chr, 
        attribute_sf = spatial_atts_ls[[dynamic_var_rsl_1L_chr]], 
        attribute_rsl_1L_chr = dynamic_var_rsl_1L_chr, data_type_chr = reference_vals_chr[2], 
        data_year_1L_chr = data_year_1L_chr, crs_nbr_dbl = crs_nbr_dbl)
    if (!is.null(reference_var_rsl_1L_chr)) {
        if (!reference_grouping_1L_chr %in% names(spatial_atts_ls[[reference_var_rsl_1L_chr]])) {
            profiled_sf <- make_intersecting_profiled_area(profiled_sf = profiled_sf, 
                profiled_sf_col_1L_chr = profiled_sf_col_1L_chr, 
                profiled_sf_row_1L_chr = profiled_sf_row_1L_chr, 
                attribute_sf = spatial_atts_ls[[reference_var_rsl_1L_chr]] %>% 
                  add_km_sqd(feature_nm_1L_chr = reference_var_rsl_1L_chr), 
                attribute_rsl_1L_chr = reference_var_rsl_1L_chr, 
                data_type_chr = reference_vals_chr[1])
        }
    }
    profiled_sf <- update_popl_counts(profiled_sf = profiled_sf, 
        group_by_var_1L_chr = group_by_var_1L_chr, dynamic_var_nm_1L_chr = reference_grouping_1L_chr, 
        data_year_1L_chr = data_year_1L_chr, dynamic_var_rsl_1L_chr = dynamic_var_rsl_1L_chr, 
        reference_var_rsl_1L_chr = reference_var_rsl_1L_chr, 
        reference_vals_chr = reference_vals_chr)
    return(profiled_sf)
}
#' Make simple features object list
#' @description make_sf_ls() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make simple features object list. The function returns Simple features object list (a list of simple features objects).
#' @param profiled_sf Profiled (a simple features object)
#' @param group_by_var_1L_chr Group by variable (a character vector of length one)
#' @return Simple features object list (a list of simple features objects)
#' @rdname make_sf_ls
#' @export 
#' @importFrom purrr map
#' @importFrom dplyr pull filter
#' @importFrom rlang sym
#' @importFrom stats setNames
#' @keywords internal
make_sf_ls <- function (profiled_sf, group_by_var_1L_chr) 
{
    sf_ls <- purrr::map(profiled_sf %>% dplyr::pull(!!rlang::sym(group_by_var_1L_chr)) %>% 
        unique(), ~profiled_sf %>% dplyr::filter(!!rlang::sym(group_by_var_1L_chr) == 
        .x)) %>% stats::setNames(profiled_sf %>% dplyr::pull(!!rlang::sym(group_by_var_1L_chr)) %>% 
        unique())
    return(sf_ls)
}
#' Make simple features object rows function
#' @description make_sf_rows_fn() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make simple features object rows function. The function returns Simple features object rows (a function).
#' @param ... Additional arguments
#' @return Simple features object rows (a function)
#' @rdname make_sf_rows_fn
#' @export 
#' @importFrom rlang dots_values
#' @importFrom sf st_sfc st_set_geometry st_sf
#' @importFrom dplyr bind_rows
#' @keywords internal
make_sf_rows_fn <- function (...) 
{
    attribution_1L_chr <- "Based on: https://github.com/r-spatial/sf/issues/49"
    sf_list <- rlang::dots_values(...)[[1]]
    sfg_list_column <- lapply(sf_list, function(sf) sf$geometry[[1]]) %>% 
        sf::st_sfc()
    df <- lapply(sf_list, function(sf) sf::st_set_geometry(sf, 
        NULL)) %>% dplyr::bind_rows()
    sf_rows_fn <- sf::st_sf(data.frame(df, geom = sfg_list_column))
    return(sf_rows_fn)
}
#' Make valid new simple features object
#' @description make_valid_new_sf() is a Make function that creates a new R object. Specifically, this function implements an algorithm to make valid new simple features object. The function returns Valid (a simple features object).
#' @param sf Simple features object (a simple features object)
#' @return Valid (a simple features object)
#' @rdname make_valid_new_sf
#' @export 
#' @importFrom dplyr filter distinct
#' @importFrom sf st_is_valid st_make_valid st_geometry_type st_collection_extract st_cast
#' @keywords internal
make_valid_new_sf <- function (sf) 
{
    valid_sf <- sf %>% dplyr::filter(sf::st_is_valid(.))
    if (nrow(valid_sf) != nrow(sf)) {
        fixed_sf <- sf %>% dplyr::filter(!sf::st_is_valid(.)) %>% 
            sf::st_make_valid()
        valid_sf <- rbind(valid_sf, fixed_sf)
    }
    gc_sf <- valid_sf %>% dplyr::filter(sf::st_geometry_type(.) == 
        "GEOMETRYCOLLECTION")
    if (nrow(gc_sf) > 0) {
        valid_sf <- gc_sf %>% sf::st_collection_extract(type = c("POLYGON")) %>% 
            rbind(valid_sf %>% dplyr::filter(sf::st_geometry_type(.) != 
                "GEOMETRYCOLLECTION"))
    }
    valid_sf <- valid_sf %>% dplyr::filter(sf::st_geometry_type(.) %in% 
        c("POLYGON", "MULTIPOLYGON")) %>% sf::st_cast("MULTIPOLYGON") %>% 
        dplyr::distinct(.keep_all = T)
    return(valid_sf)
}
