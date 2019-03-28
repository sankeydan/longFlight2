#' Group GPS trajectories for group flights
#'
#' @export
#'


gGPS = function(files,
                file_path = NULL,
                plot_speed = F,
                plot_traj = T,
                hz = 5
){

  # Objects

  # files = files2
  # file_path = out.folder
  # plot_speed = T
  # plot_traj = T
  # hz = 5

  # defensive coding
  if ( is.null ( file_path)){
    stop( "SPECIFY FILE PATH (what folder are the files in?)")
  }

  # libraries
  library(massManip)
  library(chron)

  # bird nums
  name = str_split_fixed(files,"\\.",2)[,1]

  # set up  vectors
  start.time = rep(NA,  length(files))
  nrows      = rep(NA,  length(files))

  # LOOP all files to add data to env.
  for ( j in 1:length(files)){

    #load and assign data
    load( file.path( file_path , files[j]))

    # save start time and number of rows (stored from the cut off when the individual reached home)
    start.time[j] =  data$time[1]
    nrows[j] = nrow(data)

    #assign
    assign ( paste0( "P" ,j), data)
  }

  # plot speed
  if(plot_speed){
    load(  file.path( file_path , files[1]))
    plot  (c(get_dist(data$LATITUDE, data$LONGITUDE,method = "speed",hz=5), rep(NA, max(nrows)-nrow(data))),
           type = "n",col = 1,ylab = "Speed", ylim = c(9,38))
    for ( j in 1:length(files)){
      load(file.path( file_path , files[j]))
      points(c(get_dist(data$LATITUDE, data$LONGITUDE,method = "speed",hz=5), rep(NA, max(nrows)-nrow(data))),type = "l",col = j)
    }
  }

  # Find the Min start time and match up the pigeons start times
  min.start = min(start.time)
  n.rows = rep(NA, length(files)) # for next part of script
  for ( j in 1:length(files)){
    data = get(paste0("P",j))
    foo =  matrix (NA, nrow = ( data$time[1] - min.start)*hz, ncol = ncol(data) , dimnames = list(NULL , dimnames(data)[[2]]))
    data = rbind( foo , data)
    n.rows[j] = nrow(data)
    assign( paste0("P",j) , data)
  }

  # find max end time  match up the pigeons end times
  max.fin = max(n.rows)
  for ( j in 1:length(files)){
    data = get(paste0("P",j))
    foo =  matrix (NA, nrow = ( max.fin - nrow(data)), ncol = ncol(data) , dimnames = list(NULL , dimnames(data)[[2]]))
    data = rbind( data, foo)
    assign( paste0("P",j) , data)
  }

  #build array
  assign( "data" , # specific name for each group flight
          array(NA, c( nrow(data), 3, length(files)), # rows, variables, pigeon
                dimnames = list(NULL, # dimnames , null for row
                                c("lon", "lat" , "time" ), # variable names
                                name))) # pigeon names
  for ( j in 1:length(files)){ # for each pigeon in the flock, add the following elements to the array
    data[,1,j] = get(paste0("P", j))$LONGITUDE # Longitude / x
    data[,2,j] = get(paste0("P", j))$LATITUDE # Latitude / y
    data[,3,j] = get(paste0("P", j))$time
  }

  # plot traj
  if( plot_traj){
    plot(data[,1:2,1],type="l",ylim = range(as.vector(data[,2,]),na.rm = T),
         xlim = range(as.vector(data[,1,]),na.rm = T))
    for ( i in 1:dim(data)[3]){
      lines(data[,1:2,i],col=i)
    }
  }

  #return
  return(data)
}





