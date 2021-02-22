get_normOfAVector <- function(x1, y1, x2, y2) 
{
  return(sqrt((x2-x1)^2+(y2-y1)^2))
}

getdistance_fromPointToSegment <- function(test.x, test.y, x1, y1, x2, y2) 
{
  ## Distance from (test.x,test.y) to the segment x1,y1,x2,y2
  distance <- NULL
  intersectingPoint.x <- 0
  intersectingPoint.y <- 0
  
  normOfTheSegment <- get_normOfAVector(x1, y1, x2, y2)
  if( normOfTheSegment < 0.00000001) 
  {
    warning("Short segment")
    return(9999)
  }
  u <- (((test.x - x1) * (x2 - x1)) + ((test.y - y1) * (y2 - y1)))
  u <- u / (normOfTheSegment * normOfTheSegment)
  if((u < 0.00001) || (u > 1)) 
  {
    ## closest point does not fall within the line segment, take the shorter distance to an endpoint
    intersectingPoint.x <- get_normOfAVector(test.x, test.y, x1, y1)
    intersectingPoint.y <- get_normOfAVector(test.x, test.y, x2, y2)
    if (intersectingPoint.x > intersectingPoint.y)
    {
      distance <- intersectingPoint.y
    } else 
    {
      distance <- intersectingPoint.x
    }
  } else 
  {
    ## Intersecting point is on the line, use the formula
    intersectingPoint.x <- x1 + u * (x2 - x1)
    intersectingPoint.y <- y1 + u * (y2 - y1)
    distance <- get_normOfAVector(test.x, test.y, intersectingPoint.x, intersectingPoint.y)
  }
  return(distance)
}


get_elbow_ofDistribution <- function(xPoints, yPoints)
{
  xyCoordinates <- data.frame(x=xPoints, y=yPoints)
  # Points
  point_upLeft    <- as.vector(xyCoordinates[1,])
  point_downRight <- as.vector(xyCoordinates[nrow(xyCoordinates),])
  # Segments
  diagonalSegment <- point_downRight - point_upLeft
  diagonalSegmentNormalized <- diagonalSegment/sqrt(sum(diagonalSegment^2))
  
  xyCoordinates$distToSegment <- apply(xyCoordinates, 1, function(row) { getdistance_fromPointToSegment(row[1], row[2], point_upLeft[1]$x, point_upLeft[2]$y, point_downRight[1]$x, point_downRight[2]$y)})
  
  maxDistanceToTheSegment <- xyCoordinates[which.max(xyCoordinates$distToSegment),]$x
  return(maxDistanceToTheSegment)
}



