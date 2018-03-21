


lat_lon2numeric = T
correct_time = T
correct_lon = T
get_speed = T
get_heading = T
cut_start = T
cut_end = T
cut_radius_home = 200
cut_radius_site = 1000
save = T
return_data = F
plot_traj = T
istart = 1
files = list.files(file.path (PROJHOME , "Data", "GPS"))



# Libraries

library(longFlight)
library(stringr)
library(chron)

# Files

load( file.path( PROJHOME, "Data" , "unique_flight_information.rda"))
load( file.path( PROJHOME, "Data" , "release-site-coordinates.rda"))


##### SCRIPT

## CHOOSING FILES

# In the case of group flights!

if ( cut_end == T ){ # only forgo cutting the end of the file in the case of group flights
  g_s = "all"
} else {
  g_s = "g"
}

##### READ AND MANIPULATE DATA

for ( i in istart:length(files)){ # for each chosen file

  data = read.csv( file.path (PROJHOME, "data" , "GPS", files[i]), header = T ) # read the data

  # If the data was GPX

  # sometimes I accidentely saved the data as a GPX file instead of a CSV. I then used
  # an online software "my_geoconverter" https://mygeodata.cloud/converter/ to convert.
  # in this case the following script converts data

  if(is.null(data$link1_href) == F){ # lin1_href is just a random variable output by "my_geoconverter
    # (which I paid for to get the GPX file converted into a CSV)
    data$LATITUDE = data$Y
    data$LONGITUDE = data$X
    data$UTC.DATE = str_sub( data$time, 1,10)
    data$UTC.TIME = str_sub( data$time, 12, 19)
  }

  # rename data columns

  data = as.data.frame(data [, c("LONGITUDE" , "LATITUDE" , "UTC.DATE" , "UTC.TIME") ])

  # convert lat and lon to numeric

  if(lat_lon2numeric == T){
    data$LATITUDE = suppressWarnings(as.numeric(as.character(data$LATITUDE)))
    data$LONGITUDE = suppressWarnings( as.numeric(as.character(data$LONGITUDE)))
  }

  # correct longitude -
  # sometimes stored as negative, sometimes not, but all studies conducted in negative longitude
  # so -abs(x) converts all longitude values to negative

  # correct time

  if(correct_time == T){

    if( length(which( data$UTC.TIME == "UTC TIME")) != 0){ # fault with logger, sometimes records a time
      # as "UTC TIME" instead of e.g. "00:00:01"
      data$UTC.TIME[data$UTC.TIME == "UTC TIME"] = NA # label as NA
    }
  }


  if(get_speed == T){
    data$speed = get_dist(data$LONGITUDE, data$LATITUDE, method = "speed", hz = 5)
  }

  if(get_heading == T){
    data$head = get_heading(data$LONGITUDE, data$LATITUDE, indivs = 1)
  }


  if(cut_start == T){

    if(length(which( times(data$UTC.TIME) == times(str_sub(uff(files[i], output = "start.time"), -8,-1)))) > 0){  # This cuts the data at the unique flight minimum time

      data = data[ which( times(data$UTC.TIME) == times(str_sub(uff(files[i], output = "start.time"), -8,-1)))[1]: nrow(data),]
      timefault = F
    }

    if(length(which( times(data$UTC.TIME) == times(str_sub(uff(files[i], output = "start.time"), -8,-1))))[1] == 0){  # This cuts the data at the unique flight minimum time
      print(paste( "issues with time. File = " , files[i]))
      fails[i,"time"] = 1
      timefault = T
    }

    site = rsc[rsc$site == paste(str_split_fixed(files[i], "\\.", 16)[,8],
                                 str_split_fixed(files[i], "\\.", 16)[,7], sep = ""),]
    home = rsc[rsc$site == "home",]
    data$dist2site = get_dist(data$LONGITUDE, data$LATITUDE, rep(site$lon,nrow(data)), rep(site$lat,nrow(data)), method = "distance" )
    data$dist2home = get_dist(data$LONGITUDE, data$LATITUDE, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" )


    if(is.na(which(data$dist2site > cut_radius_site)[1]) == F){
      data = data[which(data$dist2site > cut_radius_site)[1]:nrow(data),]
    }
  }

  if(cut_end == T){

    if(is.na(which(data$dist2home < cut_radius_home)[1]) == F){
      data = data[1:which(data$dist2home < cut_radius_home)[1],]
    }
    if(save == T & timefault == F & nrow(data)!=1 & is.na(which(data$dist2site > cut_radius_site)[1]) == F & is.na(which(data$dist2home < cut_radius_home)[1]) == F){
      save(data, file = file.path( PROJHOME, "Output", "mGPS", paste0(substr(files[i],1,nchar(files[i])-4) , ".rda"))) # modified GPS (mGPS)

    }
  }

  if ( plot_traj ){
    sq = seq(1,length(data$dist2site),100)
    plot(data$LATITUDE~data$LONGITUDE, type = "l", main = i, xlim = c(-0.9,-0.3), ylim = c(51.35, 51.55 ) )
    points(home$lat ~ home$lon, col = "red", pch = 2)
    points(site$lat ~ site$lon, col = "blue", pch = 2)
  }

  if( return_data == T){
    return(data)
  }


  print(paste("done with", which(files == files[i]) , "/" , length(files)))
}









