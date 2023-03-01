#' Author and save files
#' @description author.vicinity_processed() is an author method that authors and saves files to local or remote locations. This method is implemented for the ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data). The function is called for its side effects and does not return a value.
#' @param x An instance of ready4 S3 class for tibble object lookup table of meta-data for spatial data packs (imported and pre-processed data).
#' @param path_1L_chr Path (a character vector of length one)
#' @param crs_dbl Coordinates reference system (a double vector)
#' @return NULL
#' @rdname author-methods
#' @export 
#' @importFrom dplyr filter pull
#' @importFrom purrr walk
#' @importFrom ready4 author
author.vicinity_processed <- function (x, path_1L_chr, crs_dbl) 
{
    x %>% dplyr::filter(main_feature_chr == "Boundary") %>% dplyr::pull(source_reference_chr) %>% 
        purrr::walk(~readRDS(paste0(path_1L_chr, "/", .x, ".RDS")) %>% 
            transform_to_simpler_sf(crs = crs_dbl[1]) %>% saveRDS(paste0(path_1L_chr, 
            "/", .x, ".RDS")))
}
#' @rdname author-methods
#' @aliases author,vicinity_processed-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("vicinity_processed", package = "vicinity"), author.vicinity_processed)
#' Author and save files
#' @description author.vicinity_raw() is an author method that authors and saves files to local or remote locations. This method is implemented for the ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import. The function returns Files written (a logical vector).
#' @param x An instance of ready4 S3 class for tibble object lookup table of metadata about raw (un-processed) spatial data to import.
#' @param crs_dbl Coordinates reference system (a double vector), Default: numeric(0)
#' @param imports_ls Imports (a list), Default: NULL
#' @param match_vals_xx Match values (an output object of multiple potential types), Default: NULL
#' @param merge_itms_chr Merge items (a character vector), Default: character(0)
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: F
#' @param path_1L_chr Path (a character vector of length one), Default: character(0)
#' @param what_1L_chr What (a character vector of length one), Default: 'raw'
#' @return Files written (a logical vector)
#' @rdname author-methods
#' @export 
#' @importFrom ready4use assert_single_row_tb
#' @importFrom purrr reduce walk2 map_lgl
#' @importFrom sf st_geometry_type st_area
#' @importFrom dplyr mutate filter pull
#' @importFrom units set_units
#' @importFrom ready4 author
author.vicinity_raw <- function (x, crs_dbl = numeric(0), imports_ls = NULL, match_vals_xx = NULL, 
    merge_itms_chr = character(0), overwrite_1L_lgl = F, path_1L_chr = character(0), 
    what_1L_chr = "raw") 
{
    files_written_lgl <- NULL
    if (what_1L_chr %in% c("processed", "raw")) {
        ready4use::assert_single_row_tb(x)
    }
    if (what_1L_chr == "geometry") {
        ready4use::assert_single_row_tb(x)
        if (overwrite_1L_lgl | !file.exists(path_1L_chr)) {
            if (is.na(merge_itms_chr) %>% all()) {
                starter_sf <- imports_ls[[1]]
            }
            else {
                starter_sf <- purrr::reduce(merge_itms_chr, .init = imports_ls[[1]], 
                  ~make_intersecting_geometries(.x, eval(parse(text = .y)), 
                    crs_nbr_dbl = crs_dbl, validate_1L_lgl = T))
                if ((sf::st_geometry_type(starter_sf) %>% as.character() != 
                  "POINT") %>% any()) {
                  starter_sf <- starter_sf %>% dplyr::mutate(area = sf::st_area(.)) %>% 
                    dplyr::filter(area > units::set_units(0, 
                      m^2))
                }
            }
            if (x %>% dplyr::pull(main_feature_chr) == "Boundary") 
                starter_sf <- starter_sf %>% transform_to_simpler_sf(crs_dbl = crs_dbl[1])
            saveRDS(starter_sf, file = path_1L_chr)
        }
    }
    if (what_1L_chr == "processed") {
        if (x %>% dplyr::pull(data_type_chr) == "Geometry") {
            author.vicinity_raw(x, imports_ls = imports_ls, path_1L_chr = path_1L_chr, 
                merge_itms_chr = merge_itms_chr, crs_dbl = crs_dbl, 
                overwrite_1L_lgl = overwrite_1L_lgl)
        }
        if (x %>% dplyr::pull(data_type_chr) == "Attribute") {
            purrr::walk2(imports_ls, names(imports_ls), ~write_att_tb(att_tb = .x, 
                object_nm_1L_chr = .y, processed_fls_dir_1L_chr = path_1L_chr, 
                overwrite_1L_lgl = overwrite_1L_lgl))
        }
    }
    if (what_1L_chr == "raw") {
        files_written_lgl <- purrr::map_lgl(match_vals_xx, ~authorData.vicinity_raw(x, 
            path_1L_chr = path_1L_chr, data_match_value_xx = .x, 
            overwrite_1L_lgl = overwrite_1L_lgl))
    }
    return(files_written_lgl)
}
#' @rdname author-methods
#' @aliases author,vicinity_raw-method
#' @importFrom ready4 author
methods::setMethod("author", methods::className("vicinity_raw", package = "vicinity"), author.vicinity_raw)
#' 
#' Author and save files
#' @name author-VicinityLocalProcessed
#' @description author method applied to VicinityLocalProcessed
#' @param x An object of class VicinityLocalProcessed
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @param return_r4_1L_lgl Return ready4 S4 (a logical vector of length one), Default: T
#' @return Return (an output object of multiple potential types)
#' @rdname author-methods
#' @aliases author,VicinityLocalProcessed-method
#' @export 
#' @importFrom ready4use assert_single_row_tb
#' @importFrom stats setNames
#' @importFrom ready4 author
methods::setMethod("author", "VicinityLocalProcessed", function (x, crs_nbr_dbl, return_r4_1L_lgl = T) 
{
    vicinity_raw_r3 <- x@a_VicinityLookup@vicinity_raw_r3
    ready4use::assert_single_row_tb(vicinity_raw_r3)
    imports_ls <- ingest(x = vicinity_raw_r3, imports_chr = x@imports_chr, 
        data_type_1L_chr = vicinity_raw_r3$data_type, raw_fls_dir_1L_chr = x@raw_fls_dir_1L_chr, 
        processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr, 
        write_1L_lgl = x@write_1L_lgl) %>% stats::setNames(x@imports_chr)
    if (vicinity_raw_r3$data_type == "Geometry") {
        path_to_seed_sf_1L_chr <- make_paths_to_fls_for_ingest(processed_fls_dir_1L_chr = x@processed_fls_dir_1L_chr, 
            name_chr = names(imports_ls)[1], data_type_chr = "Geometry")
    }
    else {
        path_to_seed_sf_1L_chr <- x@processed_fls_dir_1L_chr
    }
    author.vicinity_raw(vicinity_raw_r3, imports_ls = imports_ls, 
        path_1L_chr = path_to_seed_sf_1L_chr, merge_itms_chr = x@merge_itms_chr, 
        crs_dbl = crs_nbr_dbl, overwrite_1L_lgl = x@overwrite_1L_lgl, 
        what_1L_chr = "processed")
    if (return_r4_1L_lgl) 
        return_xx <- renewSlot(x, "path_to_seed_sf_1L_chr", path_to_seed_sf_1L_chr) %>% 
            renewSlot("imports_ls", imports_ls)
    return(return_xx)
})
#' 
#' Author and save files
#' @name author-VicinityLocalRaw
#' @description author method applied to VicinityLocalRaw
#' @param x An object of class VicinityLocalRaw
#' @param processed_fls_dir_1L_chr_chr Processed files directory length one character vector (a character vector)
#' @param crs_nbr_dbl Coordinates reference system number (a double vector)
#' @return Y (Look up tables for spatiotemporal data)
#' @rdname author-methods
#' @aliases author,VicinityLocalRaw-method
#' @export 
#' @importFrom ready4 author
methods::setMethod("author", "VicinityLocalRaw", function (x, processed_fls_dir_1L_chr_chr, crs_nbr_dbl) 
{
    y_VicinityLookup <- authorData(x, return_r4_1L_lgl = T) %>% 
        renewSlot("processed_fls_dir_1L_chr", processed_fls_dir_1L_chr_chr) %>% 
        author(crs_nbr_dbl = crs_nbr_dbl) %>% metamorphose()
    return(y_VicinityLookup)
})
