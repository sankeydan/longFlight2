#' gGPS
#'
#'
#' This function is slow and can be optimised by not loading the entire csv. I.e. only the stuff I need.
#'

# combining the group GPSs into new RDAs for analysis and plots

# Set working directory

setwd( file.path( PROJHOME, "Output" , "mGPS"))

# libraries

library(chron)
library(stringr)

# choose files

files = list.files(file.path( PROJHOME, "Output", "mGPS"))

# objects

load(files[1])
start.time = rep(  as.POSIXct( paste(data$UTC.DATE[1], data$UTC.TIME[1])),length(files)) # This is necessary to set the "class" of start.time
nrows = rep(NA , length(files))
for ( j in 1:length(files)){ # for each pigeon in the flock


  # Load data  and assign their data to an object
  load(files[j])
  assign ( paste0( "P" ,j), data)
  date = get(paste0("P",j ))$UTC.DATE

  # correct for a bug where the data comes out as "UTC DATE"
  if ( length(levels(date))!= 1) {
    assign ( paste0( "P" ,j), get(paste0( "P" ,j))[date == names(table (date)[rev(order(table(date)))[1]]),])
  }

  # What are the start and end times
  start.time[j] =  as.POSIXct( paste(get(paste0( "P" ,j))$UTC.DATE[1], get(paste0( "P" ,j))$UTC.TIME[1])) # and Find their start time
  nrows[j] = nrow(data)
}

# Start and end times

max.start = max(start.time)+1 # Find the MAx start time, and +1 because there are 5hz for every second in our data, and "max.start" may not correspond to the first HZ. Important to start each individual on the first HZ of the second.
maxnrows = max(na.omit( nrows))     # How many rows?


#build array

assign( paste0("g2.34") , # specific name for each group flight
        array(NA, c( maxnrows, 5, length(files)), # rows, variables, pigeon
              dimnames = list(NULL, # dimnames , null for row
                              c("lon", "lat" , "time" , "head" , "speed"), # variable names
                              c(paste( metrics_row(files , output = "pigeon")))))) # pigeon names
g.temp = array(NA, c(maxnrows, 5, length(files))) # and a temporary array. (this is because we cannot use get() function to assign data in loop below)

for ( j in 1:length(files)){ # for each pigeon in the flock, add the following elements to the array

  g.temp[1:nrows[j],1,j] = get(paste0("P", j))$LONGITUDE # Longitude / x
  g.temp[1:nrows[j],2,j] = get(paste0("P", j))$LATITUDE # Latitude / y
  g.temp[1:nrows[j],3,j] =
  as.integer(as.POSIXct( paste(get(paste0( "P" ,j))$UTC.DATE, get(paste0( "P" ,j))$UTC.TIME))) # seconds since 1970
  g.temp[1:nrows[j],4,j] = get(paste0("P", j))$head # Heading
  g.temp[1:nrows[j],5,j] = get(paste0("P", j))$speed # Speed (m/s)

}

# now turn back into full array

for ( j in 1:length(files)){ # for each pigeon
  for ( k in 1:length(g.temp[1,,1])){ # For the number of variables exracted from the original dataframes

    eval(parse(text=paste(paste0("g2.34" , "[,k,j]") , "=" , "g.temp[,k,j]")))
    # This is a really nice bit of code which eliminates the problem of not being able to assign to a get() object
  }
}

data = get("g2.34" ) # cannot save a get() function.

save( data, file = file.path(PROJHOME , "Output", "gGPS" , paste0("g2.34.rda"))) # file.path() is the same as paste0 as far as I'm aware.

