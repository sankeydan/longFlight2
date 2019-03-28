#' Manipulate (clean) GPS
#'
#' @export

mGPS = function (
  data,
  cut_radius_site = 1000,
  cut_radius_home = 1000,
  plot_traj = T,
  remove_outliers_speed = 100, # more than 100m/s is outlier
  remove_outliers_deg = 2, # more than n degrees (lon/lat) from median (lon/lat)
  hz =5,
  site = "E8"
){

  #############################

  ## objects

  # cut_radius_site = 1000
  # cut_radius_home = 1000
  # plot_traj = T
  # remove_outliers_speed = 100
  # remove_outliers_deg = 2
  # hz = 5
  # site = "E8"

  #libraries
  library(massManip)
  library(plyr)
  library(stringr)

  # load metadata
  load(file.path ( PROJHOME , "Data", "release-site-coordinates.rda"))
  site = rsc[rsc$site == site,]
  home = rsc[rsc$site == "home",]
  rm(rsc)

  # rename data columns
  data = as.data.frame(data [, c("LONGITUDE" , "LATITUDE" , "UTC.DATE" , "UTC.TIME") ])
  head(data)

  # convert lat and lon to numeric
  data$LATITUDE = suppressWarnings(as.numeric(as.character(data$LATITUDE)))
  data$LONGITUDE = suppressWarnings( as.numeric(as.character(data$LONGITUDE)))

  # correct longitude -
  if(length( sign(data$LONGITUDE ) == -1 ) < # sometimes stored as negative, sometimes not, but all studies conducted in negative longitude so -(x) converts all longitude values to negative. Though in some rare cases, the birds flew  past 0 longitude, so see if length is different solves this.
     length( sign(data$LONGITUDE ) ==  1 )){
    data$LONGITUDE = -(data$LONGITUDE)
  }

  # correct time
  if( length(which( data$UTC.TIME == "UTC TIME")) != 0){ # fault with logger, sometimes records a time as "UTC TIME" instead of e.g. "00:00:01"
    data$UTC.TIME[data$UTC.TIME == "UTC TIME"] = NA
  }

  # remove outliers - using speed, and deg
  out.speed = which(get_dist(data$LONGITUDE, data$LATITUDE, hz = 5, method = "speed")>remove_outliers_speed)
  out.lon   = which(abs(data$LONGITUDE - median(data$LONGITUDE,na.rm=T)) > remove_outliers_deg)
  out.lat   = which(abs(data$LATITUDE  - median(data$LATITUDE ,na.rm=T)) > remove_outliers_deg)
  out = unique(c(out.speed, out.lat, out.lon))
  data[out,c("LONGITUDE","LATITUDE")] = NA

  # trim start / fin
  dist2 = data.frame ( site = get_dist(data$LONGITUDE, data$LATITUDE, rep(site$lon,nrow(data)), rep(site$lat,nrow(data)), method = "distance" ),
                       home = get_dist(data$LONGITUDE, data$LATITUDE, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" ))
  if( na.omit(dist2$site)[1] > cut_radius_site){ # to use if some GPS data was recorded back at the loft. This will trim that part off.
    cut_home = which(dist2$site<cut_radius_site)[1]
    data = data[cut_home+1:nrow(data),]
    dist2=dist2[cut_home+1:nrow(data),]
  }
  data = data[which(dist2$site > cut_radius_site)[1]:
                which(dist2$home < cut_radius_home)[1],]

  # remove duplicate timestamps,
  mat = str_split_fixed(data$UTC.TIME, ":", 3)
  multiplier = c(3600,60,1)
  mat = matrix( as.numeric( mat[!is.na(mat[,1]),]),ncol=3)
  data$time = apply( mat, 1, function(x){ sum(x*multiplier)})
  tb = table( data$time  )
  data$decimal.seconds= unlist(apply(t(tb),2 , function(x){ (0:(x-1))/hz } ))
  if(any(data$decimal.seconds >0.8)){
    data = data[-which(data$decimal.seconds >0.8),]
  }
  data = data[complete.cases(data),]

  # fill in missing timestamps
  data$time = data$time+data$decimal.seconds
  t.diff = data$time[2:nrow(data)] - data$time[1:(nrow(data)-1)]
  rm = which(t.diff >0.3)
  if(length(rm )>0){
    data = data[-rm,]
  }
  time = seq( data$time[1] , max(data$time,na.rm=T), 0.2)

  # time as posixct
  data$time.posixct = as.POSIXct( paste(data$UTC.DATE,data$UTC.TIME), format = "%Y/%m/%d %H:%M:%S")

  # remove other columns
  data = data [ , -which( names(data) %in% c( "UTC.TIME", "UTC.DATE", "decimal.seconds"))]

  # add na's to missing timestamp
  missing.rows = round((data$time[2:nrow(data)] - data$time[1:(nrow(data)-1)]  - (1/hz) )*hz,2)
  whi.miss = which(missing.rows != 0)
  num.miss = missing.rows[whi.miss]
  whi.miss = whi.miss + cumsum(num.miss)-num.miss[1]
  nadata = data[1,]
  nadata[1,] = NA
  if( length(whi.miss) >0){
    for( i in 1:length(whi.miss) ){
      for ( j in 1:num.miss[i]){
        # i=1
        # j=1
        topdata    = data[1:whi.miss[i],]
        bottomdata = data[(whi.miss[i]+1):nrow(data),]
        data = rbind(topdata,nadata,bottomdata)
      }
    }
  }
  data$time = time


  # plot
  if( plot_traj ){
    plot(data$LONGITUDE, data$LATITUDE,
         xlim = range ( c( data$LONGITUDE , site$lon, home$lon), na.rm = T),
         ylim = range ( c( data$LATITUDE  , site$lat, home$lat), na.rm = T))
    points( site$lon, site$lat , pch= 19, col = "red")
    points( home$lon, home$lat , pch= 19, col = "blue")
  }

  #remove a few rows so there are no NA's at start
  data = data[(hz+1):nrow(data),]

  #save
  return( data)

}

