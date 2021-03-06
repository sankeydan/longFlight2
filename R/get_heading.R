#' get_heading
#'
#' get the heading between two GPS coordinates
#'
#' @param lon1 lonitude of individual 1
#' @param lon2 lonitude of individual 2
#' @param lat1 latitude of individual 1
#' @param lat2 latitude of individual 2
#' @param indivs If indivs = 1 you will not need lat2 and lon2
#' @export

get_heading = function(lon1, lat1, lon2, lat2, indivs = 2){

  if (indivs == 1){
    lat2 = lat1[2:length(lat1)]
    lon2 = lon1[2:length(lon1)]
    lat1 = lat1[1:length(lat2)]
    lon1 = lon1[1:length(lon2)]


    ta <- lat1
    tb <- lat2
    dl <- lon2 - lon1
    X <- cos(tb)*sin(dl)
    Y <- (cos(ta)*sin(tb))-(sin(ta)*cos(tb)*cos(dl))
    return(c(NA,atan2(X,Y)))

  } else {
    ta <- lat1
    tb <- lat2
    dl <- lon2 - lon1
    X <- cos(tb)*sin(dl)
    Y <- (cos(ta)*sin(tb))-(sin(ta)*cos(tb)*cos(dl))
    return(atan2(X,Y))
  }
}

