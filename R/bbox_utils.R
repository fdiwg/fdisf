#' @name bbox_to_sf
#' @title bbox_to_sf
#' @description Creates a \pkg{sf} object out of a bounding box
#' 
#' @param xmin xmin
#' @param ymin ymin
#' @param xmax xmax
#' @param ymax ymax
#' @param crs Defaut is 4326
#' @return an object of class \code{sf}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
bbox_to_sf <- function(xmin, ymin, xmax, ymax, crs = 4326){
  pts = matrix(c(
    xmin, ymin,
    xmin, ymax,
    xmax, ymax,
    xmax, ymin,
    xmin, ymin
  ), ncol=2, byrow=TRUE)
  poly = sf::st_sf(geom = sf::st_sfc(sf::st_polygon(list(pts)), crs = crs))
  return(poly)
}

#' @name optimize_bbox
#' @title optimize_bbox
#' @description Creates an optimized bbox for an \pkg{sf} object. The optimization
#' consists in extending the bounding box to manage features crossing the dateline.
#' 
#' @param sf object of class \pkg{sf}
#' @return an object of class \code{bbox}
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' @export
#'
optimize_bbox <- function(sf){
  
  nat_bbox <- sf::st_bbox(sf)
  if(round(nat_bbox[1]) == -180 && round(nat_bbox[3]) == 180){
    global_view <- bbox_to_sf(-180,-90,180,90)
    atl_view <- bbox_to_sf(-65,-90,90,90)
    if(!sf::st_intersects(atl_view, sf, sparse = F)[1,1]){
      bboxMinX <- 0; bboxMaxX <- 0; bboxMinY <- 0; bboxMaxY <- 0
      maxNegX <- -180; maxPosX <- 180;
      for(i in 1:nrow(sf)){
        f_poly <- sf[i,]
        f_poly_bbox <- sf::st_bbox(f_poly)
        minX <- f_poly_bbox[1]
        maxX <- f_poly_bbox[3]
        minY <- f_poly_bbox[2]
        maxY <- f_poly_bbox[4]
        if(i==1){
          bboxMinX <- minX; bboxMaxX <- maxX; bboxMinY <- minY; bboxMaxY <- maxY
        }else{
          if (minX < bboxMinX) bboxMinX = minX;
          if (minY < bboxMinY) bboxMinY = minY;
          if (maxX > bboxMaxX) bboxMaxX = maxX;
          if (maxY > bboxMaxY) bboxMaxY = maxY;
        }
        if (maxX > maxNegX & maxX < 0) maxNegX = maxX;
        if (minX < maxPosX & minX > 0) maxPosX = minX;
      }
      
      #final bbox adjustment
      #in case maxNegX & maxPosX unchanged
      if (maxNegX == -180) maxNegX = -90;
      if (maxPosX == 180) maxPosX = 90;
      
      #for date-limit geographic distributions
      if (maxNegX < -65 && maxPosX > 90) {
        bboxMinX = maxPosX;
        bboxMaxX = 360 - abs(maxNegX);
      }
      
      #control for globally distributed layers
      if (bboxMinX < -175.0 && bboxMaxX > 175.0) {
        bboxMinX = -180.0;
        bboxMaxX = 180.0;
        bboxMinY = -90.0;
        bboxMaxY = 90.0;
      }
      
      #control for overlimit latitude
      if (bboxMinY < -90) bboxMinY = -90;
      if (bboxMaxY > 90) bboxMaxY = 90;
      
      #optimized bbox
      nat_bbox <- c(bboxMinX, bboxMinY, bboxMaxX, bboxMaxY)
      class(nat_bbox) = "bbox"
    }
  }
  return(nat_bbox)
}