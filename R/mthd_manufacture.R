#' Manufacture a new object
#' @description manufacture.vicinity_parameters() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment. The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object that stores simulation structural parameters relating to the spatial environment.
#' @param y_vicinity_mapes PARAM_DESCRIPTION, Default: NULL
#' @param n_its_int N its (an integer vector), Default: integer(0)
#' @param joint_dstr_1L_lgl Joint distribution (a logical vector of length one), Default: T
#' @param what_1L_chr What (a character vector of length one), Default: 'values'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom dplyr bind_rows
#' @importFrom ready4 manufacture
manufacture.vicinity_parameters <- function (x, y_vicinity_mapes = NULL, n_its_int = integer(0), 
    joint_dstr_1L_lgl = T, what_1L_chr = "values") 
{
    if (what_1L_chr == "values") {
        param_vals_tb <- reckon(x, n_its_int = n_its_int)
        if (!is.null(y_vicinity_mapes)) {
            mape_vals_tb <- reckon(y_vicinity_mapes, n_its_int = n_its_int, 
                joint_dstr_1L_lgl = joint_dstr_1L_lgl)
            param_vals_tb <- dplyr::bind_rows(param_vals_tb, 
                mape_vals_tb)
        }
        object_xx <- param_vals_tb
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_parameters-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_parameters", package = "vicinity"), manufacture.vicinity_parameters)
#' Manufacture a new object
#' @description manufacture.vicinity_points() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes. The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of the longitude and latitude cordinates of sites of services / homes.
#' @param bands_1L_dbl Bands (a double vector of length one), Default: numeric(0)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector), Default: numeric(0)
#' @param land_sf Land (a simple features object), Default: NULL
#' @param metres_1L_dbl Metres (a double vector of length one), Default: numeric(0)
#' @param service_1L_chr Service (a character vector of length one), Default: character(0)
#' @param time_min_1L_dbl Time minimum (a double vector of length one), Default: numeric(0)
#' @param time_max_1L_dbl Time maximum (a double vector of length one), Default: numeric(0)
#' @param time_steps_1L_dbl Time steps (a double vector of length one), Default: numeric(0)
#' @param travel_mode_1L_chr Travel mode (a character vector of length one), Default: 'car'
#' @param type_1L_chr Type (a character vector of length one), Default: 'single'
#' @param what_1L_chr What (a character vector of length one), Default: 'geometric'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom dplyr filter select pull arrange
#' @importFrom sf st_as_sf st_transform st_buffer st_union st_intersection st_sf
#' @importFrom purrr map
#' @importFrom stats setNames
#' @importFrom ready4 manufacture
manufacture.vicinity_points <- function (x, bands_1L_dbl = numeric(0), crs_nbr_dbl = numeric(0), 
    land_sf = NULL, metres_1L_dbl = numeric(0), service_1L_chr = character(0), 
    time_min_1L_dbl = numeric(0), time_max_1L_dbl = numeric(0), 
    time_steps_1L_dbl = numeric(0), travel_mode_1L_chr = "car", 
    type_1L_chr = "single", what_1L_chr = "geometric") 
{
    if (what_1L_chr == "isochrones") {
        if (type_1L_chr == "single") {
            one_service_tb <- x %>% dplyr::filter(service_name_chr == 
                service_1L_chr)
            one_service_sf <- make_isochrones(lat_1L_dbl = one_service_tb %>% 
                dplyr::select(lat_dbl) %>% dplyr::pull(), lng_1L_dbl = one_service_tb %>% 
                dplyr::select(lng_dbl) %>% dplyr::pull(), time_min_1L_dbl = time_min_1L_dbl, 
                time_max_1L_dbl = time_max_1L_dbl, time_steps_1L_dbl = time_steps_1L_dbl, 
                travel_mode_1L_chr = travel_mode_1L_chr)
            object_xx <- one_service_sf
        }
    }
    if (what_1L_chr == "geometric") {
        if (type_1L_chr == "single") {
            distance_from_pts_sf <- sf::st_as_sf(x, coords = c("lng_dbl", 
                "lat_dbl"), crs = crs_nbr_dbl[1]) %>% sf::st_transform(crs_nbr_dbl[2])
            distance_from_pts_on_land_sf <- sf::st_buffer(distance_from_pts_sf, 
                dist = metres_1L_dbl) %>% sf::st_union() %>% 
                sf::st_intersection(land_sf %>% sf::st_transform(crs_nbr_dbl[2])) %>% 
                sf::st_transform(crs_nbr_dbl[1]) %>% sf::st_sf()
            object_xx <- distance_from_pts_on_land_sf
        }
        if (type_1L_chr == "bands") {
            distance_in_km_1L_dbl <- metres_1L_dbl * 1000
            distances_dbl <- seq(from = distance_in_km_1L_dbl/bands_1L_dbl, 
                to = distance_in_km_1L_dbl, by = distance_in_km_1L_dbl/bands_1L_dbl)
            service_clusters_chr <- x %>% dplyr::pull(cluster_name_chr) %>% 
                unique()
            service_vicinity_points_ls <- purrr::map(service_clusters_chr, 
                ~x %>% dplyr::filter(cluster_name_chr == .x)) %>% 
                stats::setNames(service_clusters_chr)
            service_clusters_by_distance_ls <- purrr::map(distances_dbl, 
                ~make_cluster_bndys(clusters_chr = service_clusters_chr, 
                  crs_nbr_dbl = crs_nbr_dbl, distance_in_km_1L_dbl = .x, 
                  land_boundary_sf = land_sf, vicinity_points_ls = service_vicinity_points_ls)) %>% 
                stats::setNames(., paste0("km_", distances_dbl, 
                  "from_service"))
            geometric_distance_by_cluster_circles <- purrr::map(1:length(service_clusters_chr), 
                ~reorder_clusters_by_distances(clusters_by_distance_ls = service_clusters_by_distance_ls, 
                  distances_dbl = distances_dbl, index_val_1L_int = .x)) %>% 
                stats::setNames(., service_vicinity_points_ls %>% 
                  names())
            geometric_distance_by_cluster_bands <- purrr::map(geometric_distance_by_cluster_circles, 
                ~transform_circles_to_bands(geomc_dist_circles_ls = .x)) %>% 
                stats::setNames(., service_vicinity_points_ls %>% 
                  names())
            geometric_distance_by_cluster_circles_merged_list <- purrr::map(geometric_distance_by_cluster_circles, 
                ~do.call(rbind, .x)) %>% stats::setNames(., service_vicinity_points_ls %>% 
                names()) %>% purrr::map(., ~.x %>% dplyr::arrange(desc(distance_in_km_dbl)))
            geometric_distance_by_cluster_bands_merged_list <- purrr::map(geometric_distance_by_cluster_bands, 
                ~do.call(rbind, .x)) %>% stats::setNames(., service_vicinity_points_ls %>% 
                names()) %>% purrr::map(., ~.x %>% dplyr::arrange(desc(distance_in_km_dbl)) %>% 
                transform_to_simpler_sf(crs_dbl = crs_nbr_dbl[1]))
            object_xx <- geometric_distance_by_cluster_bands_merged_list
        }
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_points-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_points", package = "vicinity"), manufacture.vicinity_points)
#' Manufacture a new object
#' @description manufacture.vicinity_processed() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data). The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param approximation_1L_chr Approximation (a character vector of length one), Default: 'abs'
#' @param main_incld_feature_chr Main included feature (a character vector), Default: character(0)
#' @param target_area_1L_chr Target area (a character vector of length one), Default: character(0)
#' @param target_year_1L_chr Target year (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'closest years'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom ready4 manufacture
manufacture.vicinity_processed <- function (x, approximation_1L_chr = "abs", main_incld_feature_chr = character(0), 
    target_area_1L_chr = character(0), target_year_1L_chr = character(0), 
    what_1L_chr = "closest years") 
{
    if (what_1L_chr == "closest years") {
        closest_yrs_ls <- make_closest_yrs_ls(x, main_incld_feature_chr = main_incld_feature_chr, 
            target_area_1L_chr = target_area_1L_chr, target_year_1L_chr = target_year_1L_chr, 
            approximation_1L_chr = approximation_1L_chr)
        object_xx <- closest_yrs_ls
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_processed-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_processed", package = "vicinity"), manufacture.vicinity_processed)
#' Manufacture a new object
#' @description manufacture.vicinity_raw() is a manufacture method that manufactures a novel R object using data contained in an instance of a class. This method is implemented for the ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function returns Object (an output object of multiple potential types).
#' @param x An instance of ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param crs_nbr_dbl Coordinates reference system number (a double vector), Default: NA
#' @param match_value_xx Match value (an output object of multiple potential types), Default: NULL
#' @param match_var_nm_1L_chr Match variable name (a character vector of length one), Default: character(0)
#' @param merge_itms_chr Merge items (a character vector), Default: character(0)
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @param package_1L_chr Package (a character vector of length one), Default: character(0)
#' @param processed_fls_dir_1L_chr Processed files directory (a character vector of length one), Default: character(0)
#' @param raw_fls_dir_1L_chr Raw files directory (a character vector of length one), Default: character(0)
#' @param sub_dirs_chr Sub directories (a character vector), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'output'
#' @return Object (an output object of multiple potential types)
#' @rdname manufacture-methods
#' @export 
#' @importFrom ready4use assert_single_row_tb get_valid_path_chr manufacture.ready4use_dataverses ready4use_dataverses
#' @importFrom rlang exec
#' @importFrom purrr map_chr accumulate
#' @importFrom ready4 get_from_lup_obj manufacture
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
manufacture.vicinity_raw <- function (x, crs_nbr_dbl = NA_real_, match_value_xx = NULL, match_var_nm_1L_chr = character(0), 
    merge_itms_chr = character(0), overwrite_1L_lgl = F, package_1L_chr = character(0), 
    processed_fls_dir_1L_chr = character(0), raw_fls_dir_1L_chr = character(0), 
    sub_dirs_chr = character(0), what_1L_chr = "output") 
{
    if (what_1L_chr == "ingest") {
        object_xx <- NULL
        stop("A Make Import Object Method needs to be defined for the child class of vicinity_raw.")
    }
    if (what_1L_chr == "output") {
        ready4use::assert_single_row_tb(x)
        y_VicinityLookup <- VicinityLookup()
        y_VicinityLookup <- renewSlot(y_VicinityLookup, "vicinity_raw_r3", 
            x)
        import_type_ls <- procure.vicinity_raw(x, inc_script_1L_lgl = T, 
            forced_choice_chr = NA_character_, what_1L_chr = "source")
        if (names(import_type_ls) == "script_chr") {
            make_class_fn_chr <- eval(parse(text = import_type_ls))
            script_args_ls <- list(lup_tbs_r4 = y_VicinityLookup, 
                merge_itms_chr = merge_itms_chr, processed_fls_dir_1L_chr = processed_fls_dir_1L_chr, 
                raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, pkg_1L_chr = package_1L_chr, 
                overwrite_1L_lgl = overwrite_1L_lgl, crs_nbr_dbl = crs_nbr_dbl)
            z_VicinityArguments <- rlang::exec(make_class_fn_chr, 
                !!!script_args_ls)
            object_xx <- manufacture(z_VicinityArguments)
        }
        else {
            object_xx <- VicinityLocalRaw(lup_tbs_r4 = y_VicinityLookup, 
                merge_itms_chr = merge_itms_chr, raw_fls_dir_1L_chr = raw_fls_dir_1L_chr, 
                pkg_1L_chr = package_1L_chr, overwrite_1L_lgl = overwrite_1L_lgl) %>% 
                author(processed_fls_dir_1L_chr_chr = processed_fls_dir_1L_chr, 
                  crs_nbr_dbl = crs_nbr_dbl)
        }
    }
    if (what_1L_chr == "path") {
        path_element_chr <- purrr::map_chr(c("country_chr", "area_type_chr", 
            "region_chr", "main_feature_chr", "year_chr", "inc_file_main_chr"), 
            ~ready4::get_from_lup_obj(data_lookup_tb = x, match_var_nm_1L_chr = "name_chr", 
                match_value_xx = match_value_xx, target_var_nm_1L_chr = .x, 
                evaluate_1L_lgl = FALSE))
        object_xx <- paste0(raw_fls_dir_1L_chr, "/", paste(path_element_chr, 
            collapse = "/"))
    }
    if (what_1L_chr == "paths_chr") {
        paths_chr <- purrr::map_chr(sub_dirs_chr, ~ready4::get_from_lup_obj(data_lookup_tb = x_vicinity_raw, 
            match_value_xx = match_value_xx, match_var_nm_1L_chr = match_var_nm_1L_chr, 
            target_var_nm_1L_chr = .x, evaluate_1L_lgl = FALSE))
        paths_chr <- purrr::accumulate(paths_chr, ~paste0(.x, 
            "/", .y)) %>% paste0(processed_fls_dir_1L_chr, "/", 
            .)
        object_xx <- paths_chr
    }
    if (what_1L_chr == "source") {
        assert_single_row_tb(x)
        import_type_ls <- procure(x, inc_script_1L_lgl = !is.null(script_args_ls), 
            forced_choice_chr = forced_choice_chr, what_1L_chr = "source")
        object_xx <- switch(names(import_type_ls), script_chr = rlang::exec(VicinityArguments, 
            x, !!!script_args_ls), local_chr = ready4use::get_valid_path_chr(import_type_ls[[1]]), 
            repo_chr = ready4use::manufacture.ready4use_dataverses(x %>% 
                dplyr::select(names(ready4use::ready4use_dataverses())) %>% 
                tibble::as_tibble() %>% ready4use::ready4use_dataverses()), 
            source_url_chr = url(import_type_ls[[1]]))
    }
    return(object_xx)
}
#' @rdname manufacture-methods
#' @aliases manufacture,vicinity_raw-method
#' @importFrom ready4 manufacture
methods::setMethod("manufacture", methods::className("vicinity_raw", package = "vicinity"), manufacture.vicinity_raw)
