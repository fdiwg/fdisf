#' @name create_loxodrome_line
#' @title create_loxodrome_line
#' @description Creates a loxodrome line from a straight line
#' 
#' @param features an lines object of class \code{sf}
#' @param distance distance in meters. Default is \code{1852} (1 nautical mile)
#' @return an object of class \code{sf}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
create_loxodrome_line = function(features, distance = 1852){
  
  coords = sf::st_coordinates(features)
  points = sf::st_sfc(sf::st_point(coords[1,1:2]), sf::st_point(coords[nrow(coords),1:2]), crs = 4326)
  point1 = as(points[1,],"Spatial")
  point2 = as(points[2,],"Spatial")
  
  # Compute rhumb bearing and distance
  lox_bearing <- geosphere::bearingRhumb(point1, point2)
  lox_distance <- geosphere::distRhumb(point1, point2)
  
  if(lox_distance < distance){
    warning(
      sprintf("Features length (%s m) is < to distance (%s [m]). Return input feature geometry set",
              lox_distance, distance)
    )
    return(sf::st_as_sfc(features))
  }
  
  # Create sequence of points along the loxodrome
  n_points <- round(lox_distance / distance) + 1
  fractions <- seq(0, 1, length.out = n_points)
  interpolated_points <- t(sapply(fractions, function(f) {
    geosphere::destPointRhumb(p = point1, b = lox_bearing, d = lox_distance * f)
  }))
  
  loxodrome_line <- st_linestring(interpolated_points)
  loxodrome_sf <- st_sfc(loxodrome_line, crs = 4326)
  return(loxodrome_sf)
}