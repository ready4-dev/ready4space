#' VicinityLookup
#' 
#' Look up tables for spatiotemporal data
#' 
#' @slot vicinity_abbreviations_r3 Vicinity abbreviations (a ready4 submodule)
#' @slot vicinity_raw_r3 Vicinity raw (a ready4 submodule)
#' @slot vicinity_processed_r3 Vicinity processed (a ready4 submodule)
#' @slot vicinity_resolutions_r3 Vicinity resolutions (a ready4 submodule)
#' @slot vicinity_points_r3 Vicinity points (a ready4 submodule)
#' @slot vicinity_templates_r3 Vicinity templates (a ready4 submodule)
#' @slot vicinity_identifiers_r3 Vicinity identifiers (a ready4 submodule)
#' @slot dissemination_1L_chr Dissemination (a character vector of length one)
#' @import ready4
#' @name VicinityLookup-class
#' @rdname VicinityLookup-class
#' @export VicinityLookup
#' @exportClass VicinityLookup
VicinityLookup <- methods::setClass("VicinityLookup",
contains = "Ready4Module",
slots = c(vicinity_abbreviations_r3 = "vicinity_abbreviations",vicinity_raw_r3 = "vicinity_raw",vicinity_processed_r3 = "vicinity_processed",vicinity_resolutions_r3 = "vicinity_resolutions",vicinity_points_r3 = "vicinity_points",vicinity_templates_r3 = "vicinity_templates",vicinity_identifiers_r3 = "vicinity_identifiers",dissemination_1L_chr = "character"),
prototype =  list(vicinity_abbreviations_r3 = vicinity_abbreviations(),vicinity_raw_r3 = vicinity_raw(),vicinity_processed_r3 = vicinity_processed(),vicinity_resolutions_r3 = vicinity_resolutions(),vicinity_points_r3 = vicinity_points(),vicinity_templates_r3 = vicinity_templates(),vicinity_identifiers_r3 = vicinity_identifiers()))


methods::setValidity(methods::className("VicinityLookup"),
function(object){
msg <- NULL
if (is.null(msg)) TRUE else msg
})
