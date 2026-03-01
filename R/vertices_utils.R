#' @name add_vertices
#' @title add_vertices
#' @description Densify a spatial polygon by adding vertices
#' 
#' @param sf an object of class \code{sf}
#' @param each the step value to use to create vertices
#' @param parallel run in parallel
#' @param ... parallel options
#' @return an object of class \code{sf}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
add_vertices <- function(sf, each = 0.1, parallel = FALSE, ...){
  
  applyHandler <- if(parallel) parallel::mclapply else lapply
  
  sf <- do.call("rbind", applyHandler(1:nrow(sf), function(i){
    p = sf[i,]
    coords <- sf::st_coordinates(p)
    newcoords <- do.call("rbind",lapply(1:(nrow(coords)-1), function(i){
      i_coords <- coords[i:(i+1),]
      out_coords <- data.frame(
        x = seq(from = i_coords[1L,1L], to = i_coords[2L,1L],
                by = ifelse(i_coords[1L,1L]<=i_coords[2L,1L], each, -each)),
        y = seq(from = i_coords[1L,2L], to = i_coords[2L,2L],
                by = ifelse(i_coords[1L,2L]<=i_coords[2L,2L], each, -each))
      )
      if(i<(nrow(coords)-1)) out_coords <- out_coords[,-nrow(out_coords)]
      out_coords <- as.matrix(out_coords)
      return(out_coords)
    }))
    p$geometry <- sf::st_polygon(list(newcoords)) %>% sf::st_sfc(crs = 4326)
    return(p)
  }, ...))
  return(sf)
}

#' @name distance_between_vertices
#' @title distance_between_vertices
#' @description Calculates the distance between each pair of coordinate. The distance
#' is computed with the \link[geosphere]{distGeo} function.
#' 
#' @param sf an object of class \code{sf}
#' @return an object of class \code{data.frame} including vertices indexes and the distance
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
distance_between_vertices = function(features){
  coords = sf::st_coordinates(features)
  do.call(rbind, lapply(1:(nrow(coords)-1), function(i){
    data.frame(
      from = i,
      to = i+1,
      dist = geosphere::distGeo(sf::st_linestring(coords[i:(i+1),1:2]))[1]
    )
  }))
}

#' @name bearing_between_vertices
#' @title bearing_between_vertices
#' @description Calculates the bearing between each pair of coordinate. The bearing
#' is computed with the \link[geosphere]{bearingRhumb} function.
#' 
#' @param sf an object of class \code{sf}
#' @return an object of class \code{data.frame} including vertices indexes and the bearing
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
bearing_between_vertices = function(features){
  coords = sf::st_coordinates(features)
  do.call(rbind, lapply(1:(nrow(coords)-1), function(i){
    data.frame(
      from = i,
      to = i+1,
      dist = geosphere::bearingRhumb(
        as(sf::st_sfc(sf::st_point(coords[i,1:2]), crs = 4326), "Spatial"),
        as(sf::st_sfc(sf::st_point(coords[i+1,1:2]), crs = 4326), "Spatial")
      )
    )
  }))
}