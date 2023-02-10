#' VicinityLookup
#' @name VicinityLookup
#' @description An S4 class to represent Look up tables to use throughout ready4 suite
setOldClass(c("ready4_sp_abbreviations_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("vicinity_raw","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_data_pack_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("vicinity_resolutions","tbl_df", "tbl", "data.frame"))
setOldClass(c("vicinity_points","tbl_df", "tbl", "data.frame"))
setOldClass(c("vicinity_templates","tbl_df", "tbl", "data.frame"))
setOldClass(c("vicinity_identifiers","tbl_df", "tbl", "data.frame"))
#' @slot sp_abbreviations_lup ready4_sp_abbreviations_lup
#' @slot sp_import_lup vicinity_raw
#' @slot sp_data_pack_lup ready4_sp_data_pack_lup
#' @slot sp_resolution_lup vicinity_resolutions
#' @slot sp_site_coord_lup vicinity_points
#' @slot sp_starter_sf_lup vicinity_templates
#' @slot sp_uid_lup vicinity_identifiers
VicinityLookup <- methods::setClass("VicinityLookup",
slots = c(sp_abbreviations_lup = "ready4_sp_abbreviations_lup",sp_import_lup = "vicinity_raw",sp_data_pack_lup = "ready4_sp_data_pack_lup",sp_resolution_lup = "vicinity_resolutions",sp_site_coord_lup = "vicinity_points",sp_starter_sf_lup = "vicinity_templates",sp_uid_lup = "vicinity_identifiers"),
prototype =  list(sp_abbreviations_lup = ready4_sp_abbreviations_lup(),sp_import_lup = vicinity_raw(),sp_data_pack_lup = ready4_sp_data_pack_lup(),sp_resolution_lup = vicinity_resolutions(),sp_site_coord_lup = vicinity_points(),sp_starter_sf_lup = vicinity_templates(),sp_uid_lup = vicinity_identifiers()))


methods::setValidity(methods::className("VicinityLookup"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
