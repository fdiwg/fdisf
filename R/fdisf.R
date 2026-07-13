#' Spatial features for fisheries data interoperability
#'
#' fdisf provides a set of spatial features for fisheries data interoperability
#' 
#' @importFrom methods as
#' @import parallel
#' @import tidyr
#' @import sf
#' @importFrom terra rast
#' @importFrom terra ext
#' @importFrom terra ncell
#' @importFrom terra as.polygons
#' @importFrom geosphere destPoint
#' @importFrom geosphere bearingRhumb
#' @importFrom geosphere destPointRhumb
#' @importFrom geosphere distRhumb
#' @importFrom geosphere distGeo
#' @importFrom stringi stri_pad
#'
#' @name fdisf
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
"_PACKAGE"