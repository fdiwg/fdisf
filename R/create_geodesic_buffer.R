#' @name create_geodesic_buffer
#' @title create_geodesic_buffer
#' @description Creates a geodesic buffer
#' 
#' @param features an object of class \code{sf}
#' @param distance distance
#' @param unit unit for the distance (either 'm' for meters, or 'nm' for nautical miles)
#' @param n_segments number of segments for each single vertex buffer (intermediate step).
#' By default 72 which means a segment for each 5 degree bearing. For fine-grained buffers, values
#' could be 180 (each 2 degree) or 360 (each degree).
#' @return an object of class \code{sf}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
create_geodesic_buffer <- function(features, 
                                   distance, unit = c("m","nm"),
                                   n_segments = 72){
  unit = match.arg(unit)
  if(unit == "nm") distance = 1852*distance
  
  # Function to buffer a single point
  buffer_point <- function(coord, radius, n_segs) {
    bearings <- seq(0, 360, length.out = n_segs)
    pts <- geosphere::destPoint(coord, bearings, radius)
    sf::st_polygon(list(pts))
  }
  
  # Buffer each vertex
  vertices <- sf::st_coordinates(features)[,1:2]
  buffers <- lapply(1:nrow(vertices), function(i) {
    sf::st_sfc(buffer_point(vertices[i,], distance, n_segments), crs=4326)
  })
  
  # Union all buffers and the original polygon
  all_geoms <- sf::st_sfc(do.call(c, buffers))
  buffered <- sf::st_union(sf::st_make_valid(all_geoms))
  buffered_diff <- sf::st_difference(buffered, features)
  return(buffered_diff)
}