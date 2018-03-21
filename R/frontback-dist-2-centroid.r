##### Distance front / back

############


fb_lr_d2c = function ( data, centroid, dist2cent, hz = 5){

  ############

  #  centroid speed and heading

  centroid = cbind(centroid,
                   head = get_heading( lon1 =  centroid[,"x"], lat1 = centroid[,"y"], indivs = 1),
                   speed =   get_dist( lon1 =  centroid[,"x"], lat1 = centroid[,"y"], method = "speed" , hz = hz))
  head(centroid)



  # frontback / leftright dist 2 centroid

  frontback.res.cent = array( NA, c(nrow(data) , dim(GPS.coh)[3]))
  leftright.res.cent = array( NA, c(nrow(data) , dim(GPS.coh)[3]))

  for ( j in 1:dim(GPS.coh)[3]){
    ta <- centroid[,"x"]
    tb <- data[,"lon", j]
    dl <- data[,"lat", j] - centroid[, "y" ]
    X <- cos(tb)*sin(dl)
    Y <- (cos(ta)*sin(tb))-(sin(ta)*cos(tb)*cos(dl))
    focal.pos <- atan2(Y,X)
    focal.rel.pos.centroid <- ifelse (focal.pos-centroid[,"head"]<=-pi,
                                      focal.pos+(2*pi)-centroid[,"head"],
                                      ifelse(focal.pos-centroid[,"head"]>=pi,
                                             focal.pos-(2*pi)-centroid[,"head"],
                                             focal.pos-centroid[,"head"]))
    frontback.res.cent[,j] <- cos(focal.rel.pos.centroid)*dist2cent[,j]
    leftright.res.cent[,j] <- sin(focal.rel.pos.centroid)*dist2cent[,j]


  }

  boxplot( frontback.res.cent)
  boxplot( leftright.res.cent)

  return( list(frontback.res.cent, leftright.res.cent))
}

