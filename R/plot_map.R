
plot_map = function(data){

  data.map = list(data[,1:2,1],
                  data[,1:2,2],
                  data[,1:2,3],
                  data[,1:2,4],
                  data[,1:2,5],
                  data[,1:2,6])


  axis.nums.size = 12
  axis.labs.size = 14
  centre = NA
  maptype = "satellite"
  zoom = 9



  num.of.files = length(data.map)
  dat = as.data.frame(data.map[[1]])
  names(dat) = c("lon", "lat")
  lis = T



  centre = c ( (max( dat[ ,1] , na.rm=T) + min( dat [ ,1], na.rm = T) ) /2 ,
               (max( dat[ ,2] , na.rm=T) + min( dat [ ,2], na.rm = T) ) /2 ) # find centre for map



  map = ggmap::get_map( location = centre,
                        zoom = zoom,
                        maptype = "satellite") # load map
  p = ggmap::ggmap(map)

  rain = rainbow(6)
  for ( i in 1:6){

    if ( lis == T){
      dat = as.data.frame(data.map[[i]])
      names(dat) = c("lon", "lat")
    }
    p = p + ggplot2::geom_path (data= dat, ggplot2::aes(x=lon, y=lat),
                                color = rain[i], size = 0.5)
  }

  p = p + ggplot2::geom_path (data= dat[1:29282,], ggplot2::aes(x=lon, y=lat),
                              color = "black", size = 2)

  scale_x_longitude <- function(xmin=-180, xmax=180, step=0.2, ...) {
    xbreaks <- seq(xmin,xmax,step)
    xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"lon")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
    return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
  }

  scale_y_latitude <- function(ymin=-90, ymax=90, step=0.2, ...) {
    ybreaks <- seq(ymin,ymax,step)
    ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
    return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
  }

  p = p  + ggplot2::xlab("Longitude")+
    ggplot2::ylab("Latitude") +
    ggplot2::theme(axis.text= ggplot2::element_text(size= axis.nums.size),
                   axis.title= ggplot2::element_text(size= axis.labs.size ,face="bold"))
  print(p)

}

