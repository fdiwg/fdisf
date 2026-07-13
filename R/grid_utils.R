#' @title center_from_grid_code
#' @description
#'  Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#' Returns details about the center of a regular grid provided the grid code.
#' Details include:
#'     the center latitude / longitude (regardless of the fraction of ocean area in the grid)
#'     the grid width and height (in degrees)
#'
#' @param grid_code A grid code
#' @return A named vector containing the center latitude (\code{y}) and longitude (\code{x}) plus the grid
#' width (\code{size_lat}) and height (\code{size_lon}) in degrees
#' @examples
#' center_from_grid_code("5206066")
#' center_from_grid_code("6205065")
#' @export
center_from_grid_code = function(grid_code) {
  #Grid sizes by gridCode[1], i.e.:
  #1 -> 30x30
  #2 -> 10x20
  #3 -> 10x10
  #4 -> 20x20
  #5 ->  1x1
  #6 ->  5x5

  #Height and width (in degrees) by type of grid (i.e. gridCode[0]) sorted by lexicographical first char code
  #(1 = 30x30, 2 = 10x20, 3 = 10x10, 4 = 20x20, 5 = 1x1, 6 = 5x5)
  sizes_lat = c(30, 10, 10, 20, 1, 5)
  sizes_lon = c(30, 20, 10, 20, 1, 5)

  grid_code = toString(grid_code)

  size_code = as.numeric(substring(grid_code, 1, 1))

  quadrant = as.numeric(substring(grid_code, 2, 2))

  lat = as.numeric(substring(grid_code, 3, 4))
  lon = as.numeric(substring(grid_code, 5, 7))

  latitudes.quadrant  = c(  1, -1, -1,  1)
  longitudes.quadrant = c(  1,  1, -1,  1)

  lat = latitudes.quadrant[quadrant]  * ( lat + sizes_lat[size_code] / 2 );
  lon = longitudes.quadrant[quadrant] * ( lon + sizes_lon[size_code] / 2 );

  return (c(x = lon, y = lat, size_lat = sizes_lat[size_code], size_lon = sizes_lon[size_code]))
}


#' @title CWP_to_grid_coordinates
#' @description
#'  Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#'Converts a CWP grid code into its four boundary points (NW, NE, SW and SE)
#'@param grid_code A CWP grid code
#'@return a data table containing the coordinates (LAT, LON) of each of the four boundary points for the grid
#'@export
#'@examples CWP_to_grid_coordinates("5201123")
#'@examples CWP_to_grid_coordinates("6205125")
CWP_to_grid_coordinates <- function(grid_code) {
  
  s <- as.integer(substr(grid_code, 1, 1))
  
  dx <- dy <- 1
  
  if (s == 6) {
    dx <- dy <- 5
  } else if (s == 3) {
    dx <- dy <- 10
  } else if (s == 2) {
    dx <- 20
    dy <- 10
  } else if (s == 4) {
    dx <- dy <- 20
  } else if (s == 1) {
    dx <- dy <- 30
  }
  
  q   <- as.integer(substr(grid_code, 2, 2))
  lat <- as.integer(substr(grid_code, 3, 4))
  lon <- as.integer(substr(grid_code, 5, 7))
  
  points <- data.frame(
    POS = character(),
    LON = numeric(),
    LAT = numeric(),
    stringsAsFactors = FALSE
  )
  
  if (q == 1) {       # NE quadrant
    points <- rbind(points, data.frame(POS="NW", LON=lon,      LAT=lat+dy))
    points <- rbind(points, data.frame(POS="NE", LON=lon+dx,   LAT=lat+dy))
    points <- rbind(points, data.frame(POS="SE", LON=lon+dx,   LAT=lat))
    points <- rbind(points, data.frame(POS="SW", LON=lon,      LAT=lat))
    
  } else if (q == 2) { # SE quadrant
    points <- rbind(points, data.frame(POS="NW", LON=lon,      LAT=-lat))
    points <- rbind(points, data.frame(POS="NE", LON=lon+dx,   LAT=-lat))
    points <- rbind(points, data.frame(POS="SE", LON=lon+dx,   LAT=-lat-dy))
    points <- rbind(points, data.frame(POS="SW", LON=lon,      LAT=-lat-dy))
    
  } else if (q == 3) { # SW quadrant
    points <- rbind(points, data.frame(POS="NW", LON=-lon-dx,  LAT=-lat))
    points <- rbind(points, data.frame(POS="NE", LON=-lon,     LAT=-lat))
    points <- rbind(points, data.frame(POS="SE", LON=-lon,     LAT=-lat-dy))
    points <- rbind(points, data.frame(POS="SW", LON=-lon-dx,  LAT=-lat-dy))
    
  } else if (q == 4) { # NW quadrant
    points <- rbind(points, data.frame(POS="NW", LON=-lon-dx,  LAT=lat+dy))
    points <- rbind(points, data.frame(POS="NE", LON=-lon,     LAT=lat+dy))
    points <- rbind(points, data.frame(POS="SE", LON=-lon,     LAT=lat))
    points <- rbind(points, data.frame(POS="SW", LON=-lon-dx,  LAT=lat))
  }
  
  rownames(points) <- NULL
  points
}

#' @title convert_to_CWP_grid
#' @description
#'Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#'Converts a pair of decimal coordinate into a CWP grid code for a specific grid type
#'@param lon Longitude (decimal coordinates)
#'@param lat Latitude (decimal coordinates)
#'@param grid_type_code The type of CWP grid (one among \code{\link{grid_1x1}}, \code{\link{grid_5x5}}, \code{\link{grid_10x10}}, \code{\link{grid_10x20}}, \code{\link{grid_20x20}} and \code{\link{grid_30x30}})
#'@return The CWP grid code for the provided coordinates and grid type
#'@export
#'@examples convert_to_CWP_grid(20, -10, grid_1x1)
convert_to_CWP_grid = function(lon, lat, grid_type_code = grid_5x5) {
  q = NA

  if     (lon >=0 && lat >=0) q = 1
  else if(lon >=0 && lat < 0) q = 2
  else if(lon < 0 && lat < 0) q = 3
  else if(lon < 0 && lat >=0) q = 4

  lat = floor(abs(lat))
  lon = floor(abs(lon))

  latS = 1
  lonS = 1

  grid = grid_char_1x1

  if     (grid_type_code == grid_5x5)   { lonS = latS =  5;     grid = grid_char_5x5 }
  else if(grid_type_code == grid_10x10) { lonS = latS = 10;     grid = grid_char_10x10 }
  else if(grid_type_code == grid_20x20) { lonS = latS = 20;     grid = grid_char_20x20 }
  else if(grid_type_code == grid_30x30) { lonS = latS = 30;     grid = grid_char_30x30 }
  else if(grid_type_code == grid_10x20) { lonS = 20; latS = 10; grid = grid_char_10x20 }

  lat = floor(lat / latS) * latS
  lon = floor(lon / lonS) * lonS

  return(paste0(grid, q, stringi::stri_pad(lat, 2, pad = "0"), stringi::stri_pad(lon, 3, pad = "0")))
}


#' @title convert_CWP_grid
#' @description
#'Function copied from iotc_core_gis_cwp_utils (https://github.com/iotc-secretariat/iotc-lib-core-gis-cwp/blob/master/R/iotc_core_gis_cwp_utils.R)
#'Converts a CWP grid code into another CWP grid code of a given type
#'@param grid_code A CWP grid code
#'@param target_grid_type_code The type of CWP grid (one among \code{\link{grid_1x1}}, \code{\link{grid_5x5}}, \code{\link{grid_10x10}}, \code{\link{grid_10x20}}, \code{\link{grid_20x20}} and \code{\link{grid_30x30}})
#'@return The CWP grid code for the grid of type \code{grid_type_code} that contains the main corner of the original grid
#'@export
#'@examples convert_CWP_grid("5201123", grid_5x5)
#'@examples convert_CWP_grid("6205125", grid_1x1)
convert_CWP_grid = function(grid_code, target_grid_type_code = grid_1x1) {
  q = as.integer(substr(grid_code, 2, 2))

  qLon = qLat = 1

  if     (q == 2) { qLon =  1; qLat = -1 }
  else if(q == 3) { qLon = -1; qLat = -1 }
  else if(q == 4) { qLon = -1; qLat =  1 }

  lat = qLat * as.integer(substr(grid_code, 3, 4))
  lon = qLon * as.integer(substr(grid_code, 5, 7))

  return (convert_to_CWP_grid(lon, lat, target_grid_type_code))
}

#' @name create_cwp_grid
#' @title create_cwp_grid
#' @description Creates a CWP grid spatial object
#' 
#' @param size an integer code corresponding to the grid size (referred as code
#' A in the CWP Handbook)
#' @param res a string matching one of the accepted resolution values. Accepted 
#' resolutions values are '10min_x_10min', '20min_x_20min', '30min_x_30min',
#' '30min_x_1deg', '1deg_x_1deg', '5deg_x_5deg', '10deg_x_10deg', '20deg_x_20deg',
#' '30deg_x_30deg'"
#' @param xmin xmin of the output grid
#' @param ymin ymin of the output grid
#' @param xmax xmax of the output grid
#' @param ymax ymax of the output grid
#' @param densify densify
#' @param parallel run in parallel
#' @param ... parallel options
#' @return an object of class \code{sf}
#' 
#' @references 
#'   CWP Handbook - https://www.fao.org/cwp-on-fishery-statistics/handbook/general-concepts/main-water-areas/fr/#c737133
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
create_cwp_grid <- function(size = NULL, res = NULL,
                          xmin = NULL, ymin = NULL, xmax = NULL, ymax = NULL,
                          densify = FALSE, parallel = FALSE, ...){
  
  applyHandler <- if(parallel) parallel::mclapply else lapply
  
  #reference resolutions
  grids <- data.frame(
    size = c(1,2,3,4,5,6,7,8,9),
    lat = c(1/6, 1/3, 0.5, 0.5, 1, 5, 10, 20, 30),
    lon = c(1/6, 1/3, 0.5, 1, 1, 5, 10, 20, 30),
    res = c("10min_x_10min", "20min_x_20min","30min_x_30min", "30min_x_1deg",
            "1deg_x_1deg", "5deg_x_5deg", "10deg_x_10deg", "20deg_x_20deg", "30deg_x_30deg")
  )
  
  #bbbox
  xmin <- if(is.null(xmin)) -180 else xmin
  ymin <- if(is.null(ymin)) -90 else ymin
  xmax <- if(is.null(xmax)) 180 else xmax
  ymax <- if(is.null(ymax)) 90 else ymax
  
  #select grid resolution
  if(!is.null(size)){
    grid <- grids[grids$size == size,]
  }else{
    if(!is.null(res)){
      grid <- grids[grids$res == res,]
    }else{
      stop(sprintf("Please provide either the grid size (CWP A code) or the explicit resolution. Accepted resolutions values are %s",
                   paste(paste0("'",grids$res,"'"), collapse=", ")))
    }
  }
  
  #special case of 20deg resolution
  if(grid$size == 8){
    ymin = -80
    ymax = 80
  }
  
  r <- terra::rast(
    terra::ext(x = c(xmin, ymin, xmax,  ymax), xy = TRUE),
    nrow = length(seq(ymin,ymax, grid$lat))-1, 
    ncol=length(seq(xmin,xmax, grid$lon))-1, 
    crs = "epsg:4326"
  )    
  r[] <- 1:terra::ncell(r)
  sf <- r |>
    terra::as.polygons() |>
    sf::st_as_sf()
  
  #densify adding vertices each minute
  if(densify) sf <- add_vertices(sf, each = 1/60, parallel = parallel, ...)
  
  #attributes (including grid coding)
  idx <- 0
  attrs <- do.call("rbind", applyHandler(1:nrow(sf), function(i){
    poly <- sf[i,]
    pt = suppressWarnings(sf::st_centroid(poly))
    labpt <- pt |>
      sf::st_coordinates()
    quadrant <- paste0(ifelse(labpt[2]<0,"S","N"), ifelse(labpt[1]<0,"W","E"))
    quadrant_id <- switch(quadrant, "NE" = 1L, "SE" = 2L, "SW" = 3L, "NW" = 4L)
    corner_lon <- sprintf("%03.f", as.integer(min(abs(sf::st_bbox(poly)[c(1,3)]))))
    corner_lat <- sprintf("%02.f", as.integer(min(abs(sf::st_bbox(poly)[c(2,4)]))))
    gridcode <- paste0(grid$size, quadrant_id, corner_lat, corner_lon)
    
    cwp.idx <- NA
    if(grid$size < 5){
	    m.bbox <- sf::st_bbox(poly)
      m <- as.integer(floor(m.bbox))
      if(m[4]==m[2]) m[4] <- m[4]+1
      if(m[3]==m[1]) m[3] <- m[3]+1
      mr <- terra::rast(
        terra::ext(x = m, xy = TRUE),
        nrow = length(seq(m[2],m[4], grid$lat))-1, 
        ncol=length(seq(m[1], m[3], grid$lon))-1, 
        crs = "epsg:4326"
      ) 
      mr[] <- 1:terra::ncell(mr)
      mr.sf <- mr |>
        terra::as.polygons() |>
        sf::st_as_sf()
      mr.seq <- mr.sf[[1]]
      mr.seq <- switch(quadrant,
                       "SE" = as.character(mr.seq),
                       "NW" = as.character(rev(mr.seq)),
                       "NE" = as.character(unlist(rev(split(mr.seq, ceiling(seq_along(mr.seq)*grid$lon))))),
                       "SW" = as.character(unlist(rev(split(rev(mr.seq), ceiling(seq_along(rev(mr.seq))*grid$lon))))))
      mr.sf[[1]] <- mr.seq
      cwp.idx <- as.integer(mr.sf[as.integer(sf::st_intersects(pt, mr.sf)),][[1]])
      gridcode <- paste0(gridcode, cwp.idx)
    }
    
    df <- data.frame(GRIDTYPE = grid$res, QUADRANT = quadrant, X_COORD = labpt[1], Y_COORD = labpt[2], 
                     CWP_A = grid$size, CWP_B = quadrant_id, CWP_C = corner_lat, CWP_D = corner_lon, CWP_E = cwp.idx,
                     CWP_CODE = gridcode, SURFACE = as.numeric(sf::st_area(poly)))
    return(df)
  }, ...))
  
  sf <- cbind(sf, attrs)
  sf$lyr.1 = NULL
  return(sf)
}

#constants
### Numeric grid type codes (CL_FISHING_GROUND_TYPE primary keys)

#' A constant representing the code for a 1x1 regular grid
#' @seealso grid_code_1x1
#' @export
grid_1x1   = 1

#' A constant representing the code for a 5x5 regular grid
#' @seealso grid_code_5x5
#' @export
grid_5x5   = 2

#' A constant representing the code for a 10x10 regular grid
#' @seealso grid_code_10x10
#' @export
grid_10x10 = 3

#' A constant representing the code for a 10x20 regular grid
#' @seealso grid_code_10x20
#' @export
grid_10x20 = 4

#' A constant representing the code for a 20x20 regular grid
#' @seealso grid_code_20x20
#' @export
grid_20x20 = 5

#' A constant representing the code for a 30x30 regular grid
#' @seealso grid_code_30x30
#' @export
grid_30x30 = 6

#' A constant representing the code for an irregular grid
#' @seealso grid_code_irregular
#' @export
grid_irregular = 7

#' The list of all constants representing the codes for the various grids
#' @export
grid_ALL = c(grid_1x1, grid_5x5, grid_10x10, grid_10x20, grid_20x20, grid_30x30)

#' A constant representing the code for a 1x1 regular grid
#' @seealso grid_1x1
#' @export
grid_code_1x1   = grid_1x1

#' A constant representing the code for a 5x5 regular grid
#' @seealso grid_5x5
#' @export
grid_code_5x5   = grid_5x5

#' A constant representing the code for a 10x10 regular grid
#' @seealso grid_10x10
#' @export
grid_code_10x10 = grid_10x10

#' A constant representing the code for a 10x20 regular grid
#' @seealso grid_10x20
#' @export
grid_code_10x20 = grid_10x20

#' A constant representing the code for a 20x20 regular grid
#' @seealso grid_20x20
#' @export
grid_code_20x20 = grid_20x20

#' A constant representing the code for a 30x30 regular grid
#' @seealso grid_30x30
#' @export
grid_code_30x30 = grid_30x30

#' A constant representing the code for an irregular grid
#' @seealso grid_irregular
#' @export
grid_code_irregular = grid_irregular

#' The list of all constants representing the codes for the various grids
#' @export
grid_codes_ALL = c(grid_code_1x1, grid_code_5x5, grid_code_10x10, grid_code_10x20, grid_code_20x20, grid_code_30x30)

### Character grid type codes (CL_FISHING_GROUND_TYPE code)

#' A constant representing the initial character of a 1x1 regular grid code
#' @export
grid_char_1x1   = "5"

#' A constant representing the initial character of a 5x5 regular grid code
#' @export
grid_char_5x5   = "6"

#' A constant representing the initial character of a 10x10 regular grid code
#' @export
grid_char_10x10 = "3"

#' A constant representing the initial character of a 10x20 regular grid code
#' @export
grid_char_10x20 = "2"

#' A constant representing the initial character of a 20x20 regular grid code
#' @export
grid_char_20x20 = "4"

#' A constant representing the initial character of a 30x30 regular grid code
#' @export
grid_char_30x30 = "1"

#' The list of all constants representing the initial character for the various regular grid codes
#' @export
grid_chars_ALL = c(grid_char_1x1, grid_char_5x5, grid_char_10x10, grid_char_10x20, grid_char_20x20, grid_char_30x30)
