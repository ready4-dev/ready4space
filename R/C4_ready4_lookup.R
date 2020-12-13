#' ready4_lookup
#' @name ready4_lookup
#' @description An S4 class to represent Look up tables to use throughout ready4 suite
setOldClass(c("ready4_sp_abbreviations_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_import_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_data_pack_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_resolution_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_site_coord_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_starter_sf_lup","tbl_df", "tbl", "data.frame"))
setOldClass(c("ready4_sp_uid_lup","tbl_df", "tbl", "data.frame"))
#' @slot sp_abbreviations_lup ready4_sp_abbreviations_lup
#' @slot sp_import_lup ready4_sp_import_lup
#' @slot sp_data_pack_lup ready4_sp_data_pack_lup
#' @slot sp_resolution_lup ready4_sp_resolution_lup
#' @slot sp_site_coord_lup ready4_sp_site_coord_lup
#' @slot sp_starter_sf_lup ready4_sp_starter_sf_lup
#' @slot sp_uid_lup ready4_sp_uid_lup
ready4_lookup <- methods::setClass("ready4_lookup",
slots = c(sp_abbreviations_lup = "ready4_sp_abbreviations_lup",sp_import_lup = "ready4_sp_import_lup",sp_data_pack_lup = "ready4_sp_data_pack_lup",sp_resolution_lup = "ready4_sp_resolution_lup",sp_site_coord_lup = "ready4_sp_site_coord_lup",sp_starter_sf_lup = "ready4_sp_starter_sf_lup",sp_uid_lup = "ready4_sp_uid_lup"),
prototype =  list(sp_abbreviations_lup = ready4_sp_abbreviations_lup(),sp_import_lup = ready4_sp_import_lup(),sp_data_pack_lup = ready4_sp_data_pack_lup(),sp_resolution_lup = ready4_sp_resolution_lup(),sp_site_coord_lup = ready4_sp_site_coord_lup(),sp_starter_sf_lup = ready4_sp_starter_sf_lup(),sp_uid_lup = ready4_sp_uid_lup()))


methods::setValidity(methods::className("ready4_lookup"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
