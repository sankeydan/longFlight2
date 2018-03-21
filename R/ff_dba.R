###### Peak detection and DBA


ff_dba = function ( filename, start = NA, fin = NA, sampling_freq = 200, split.val = 100000,
                    smooth_type = c("standard", "num.flaps"), length.meets.gps = F){


  # # # # # # work

  # filename = ACCfiles[j]
  # start = start
  # fin = fin
  # sampling_freq = 200
  # split.val = 100000


  # # # # # # #


  ##############

  ## TEST??

  ##############

  #test = T
  test = F

  ##############

  ## ENVIRONMENT AND DATA

  #############

  # Libraries

  library(longFlight)
  library(signal)
  library(pracma)
  library(chron)
  library(stringr)

  # Clear workspace

  if ( test){
    rm(list = ls())
  }

  # Memory limit

  memory.limit(15000)

  # Load data

  if ( test){
    acc_data = read.csv ( file.path ( PROJHOME , "data" , "ACC", "P1.1.Fly.Gro.F.1.4.N.2.4.14097.57.2.7.1.2.csv"))
  } else {
    acc_data = read.csv ( file.path ( PROJHOME , "data" , "ACC" , filename))
  }

  names (acc_data) = c( "num" , "datetime" ,"x" , "y" , "z")
  acc_data$time = apply(t(as.character(acc_data$datetime)), 2, function(x) substr(x, nchar(x)-11, nchar(x)))

  # stop if not in the same time frame

  #
  ##
  #
  #
  #
  #
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!#
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!#
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!#
  c = 56 # this is a decoy. Don't run next line!#
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!
  c = 56 # this is a decoy. Don't run next line!#
  #
  #
  #
  #
  #
  # if(!is.na(which(substr(acc_data$time, 1,8) ==
  #                as.character(times(as.character(fin))+ times("01:00:00")) )[1])){


  st = which(substr(acc_data$time, 1,8) == as.character(times(as.character(start))+ times("01:00:00")) )[1]
  if ( is.na(st)){
    st = 1
  }

  fi = which(substr(acc_data$time, 1,8) ==
               as.character(times(as.character(fin))+ times("01:00:00")) )[1]

  if(is.na(fi)){
    fi = nrow(acc_data)
  }

  acc_data = acc_data[st:fi,]



  ################

  # PLOT

  ###############

  if ( test){

    acc_data = acc_data[20000:22200,]
    plot(acc_data$z, type = "l")
  }

  ############

  ## PEAK DETECTION

  ############


  ## Filter z-axis using running mean over 5 data

  z_acc_peaks =runningmean( acc_data$z ,smth =  5)

  ## Find max acceleration peaks (downstroke)

  min = PeakDetect(z_acc_peaks, 0.9)$min
  max = PeakDetect(z_acc_peaks, 0.9)$max

  # stationary periods

  max1 = max[1:(length(max)-1)]
  max2 = max[2:length(max)]
  max3 = max2 - max1
  max4 = which(max3> sampling_freq) # more than a second between flaps rings alarm bell

  if ( length(max4) > 0){
    max5 = max4+1

    vec = vector()
    for ( j in 1:length(max4)){

      vec2 = c(max[max4[j]]:max[max5[j]])
      vec = c(vec, vec2)
    }

    vec3 = unique(vec)

    acc_data = acc_data[-vec3,]

    ## Filter z-axis using running mean over 5 data

    z_acc_peaks =runningmean( acc_data$z ,smth =  5)

    ## Find max acceleration peaks (downstroke)

    min = PeakDetect(z_acc_peaks, 0.9)$min
    max = PeakDetect(z_acc_peaks, 0.9)$max

  }

  ############

  ## RUNNING MEAN

  ############



  # plot

  if ( test){
    plot(z_acc_peaks, type = "l")
    points(z_acc_peaks[min]~min , col = "blue")
    points(z_acc_peaks[max]~max , col = "red")
  }

  ############

  ## FLAP FREQUENCY

  ###########

  op = options(digits.secs = 3)

  # time difference between minima
  td = strptime(acc_data$time[min[2:length(min)]], "%H:%M:%OS")-
    strptime(acc_data$time[min[1:(length(min)-1)]], "%H:%M:%OS")

  # flap frequency
  ff = 1/as.numeric(td)

  if ( test){
    print(ff)
  }


  ############

  ## REMOVE GRAVITY

  ############



  # Running mean over 15 flaps

  if (test){
    plot(z_acc_peaks, type = "l")
    lines(runningmean(x = NULL, xyz = runmean5, smth = 15, maxima =  max)[,3], col = "red") # If xyz is used in runningmean, matrix is returned. Therefore, use [,3] to specify the z-axis
  }


  times = ceiling ( nrow(acc_data)/ split.val) # how many times do we need to split our data?  (Because of super low running speeds and in some cases, impossible to run code with my laptop if too large)


  if ( times >1 ){ # If more than once
    gravity = vector() # set up a vector
    for ( j in 1:times){ # and for each split
      obj = z_acc_peaks[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
      max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector

      if ( smooth_type[1] == "num.flaps" ){
        gravity = c(gravity,
                    runningmean(obj,
                                smth_type = "num.flaps" ,
                                smth = 15,
                                maxima =  max.loop,
                                split.val = split.val,
                                j = j)
        )# use running mean function, and combine to previous splits
      } else {
        gravity = c(gravity,runningmean(obj, smth_type = "standard" , 401, max.loop))
      }
      # print(paste ( j , "/" , times))
    }
  } else { # if the data do not need to be split.

    if ( smooth_type[1] == "num.flaps" ){
      gravity = runningmean(z_acc_peaks, smth_type = "num.flaps" , 15, max ) # Do the same
    } else {
      gravity = runningmean(z_acc_peaks, smth_type = "standard" , 401, max )
    }
  }


  # plot gravity

  if( test ) {
    plot( gravity[ sort( sample ( 1:length(gravity),2000))] )
  }

  # Remove gravity

  ACC_rmean = data.frame( z = z_acc_peaks - gravity[1:length(z_acc_peaks)])


  # Plot

  if ( test){
    plot(ACC_rmean$z,type = "l")
    points(ACC_rmean$z[min]~min , col = "blue")
    points(ACC_rmean$z[max]~max , col = "red")
  }



  # new max and min values at peaks (maxtab)

  maxtab = ACC_rmean$z[max]
  mintab = ACC_rmean$z[min]


  # plot

  if ( test){
    plot(ACC_rmean[,1] , type = "n" )
    for ( i in c(1)){
      lines(ACC_rmean[,i], col = i+1)
    }
  }

  # Does this figure look ok? Is the axis centred around zero?


  ############

  # CONVERT TO M/S2 FOR DISPLACMENT

  ############

  z_acc_rmean_ms = rep(NA, nrow(acc_data))

  z_acc_rmean_ms[!is.na(ACC_rmean$z)] = na.omit(ACC_rmean$z) * 9.80665

  if (  test ){
    plot(z_acc_rmean_ms[sort(sample(1:length(z_acc_rmean_ms),2000))])
  }

  ############

  # CALCULATE DISPLACEMENT USING DOUBLE INTEGRATION OF DORSAL MEASUREMENTS

  ###########

  # Calculate velocity, first integration

  acc_time = acc_data$time
  Time = as.numeric(strptime(acc_time, "%H:%M:%OS")) # convert time to numeric

  z_velocity_rmean = rep(NA, nrow(acc_data))

  only = !is.na(z_acc_rmean_ms) # only non NA

  # integrate

  z_velocity_rmean[only] =pracma::cumtrapz(Time[only], as.vector(z_acc_rmean_ms[only]))

  if ( test ){
    plot(z_velocity_rmean[sort(sample(1:length(z_velocity_rmean),2000))])
  }
  velocity_rmean = data.frame(z = z_velocity_rmean) # and into data frame

  # running mean


  if ( times >1 ){ # If more than once
    rmeanVel = vector() # set up a vector
    for ( j in 1:times){ # and then, for each split
      obj = velocity_rmean$z[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
      max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector

      if ( smooth_type[1] == "num.flaps" ){
        rmeanVel = c(rmeanVel, runningmean(obj, smth_type = "num.flaps" , 15, max.loop, split.val = split.val , j = j)) # use running mean function, and combine to previous splits
      } else {
        rmeanVel = c(rmeanVel,runningmean(obj, smth_type = "standard" , 401, max.loop))
      }
      #     print(paste ( j , "/" , times))
    }
  } else { # if the data do not need to be split.

    if ( smooth_type[1] == "num.flaps" ){
      rmeanVel = runningmean(velocity_rmean$z, smth_type = "num.flaps" , 15, max ) # Do the same
    } else {
      rmeanVel = runningmean(velocity_rmean$z, smth_type = "standard" , 401, max )
    }
  }

  # plot if test

  if ( test) {
    plot(velocity_rmean[,1], type = "l")
    lines(rmeanVel)

  }

  # remove running mean velocity ( drift)

  velocity_rmean_removed =  velocity_rmean$z - rmeanVel[1:length(velocity_rmean$z)]

  if ( test ){
    plot(velocity_rmean_removed[sort(sample(1:length(velocity_rmean_removed),2000))])
  }

  # From Lucy's paper:
  # "After the first integration, a running mean over 15
  # wingbeat cycles was removed from velocity to remove drift"


  # Second integration (displacement)


  z_displacement_rmean = rep(NA, nrow(acc_data))
  only = !is.na(velocity_rmean_removed)
  z_displacement_rmean[only] =cumtrapz(Time[only], as.vector(velocity_rmean_removed[only]))
  displacement_rmean = data.frame(z = z_displacement_rmean) # and into data frame

  # running mean to remove changes in displacement


  if ( times >1 ){ # If more than once
    rmeanDis = vector() # set up a vector
    for ( j in 1:times){ # and then, for each split
      obj = displacement_rmean$z[ ((split.val * (j -1))+1) : (split.val * j)] # split the data
      max.loop = max[max>((split.val * (j -1))+1) & max<(split.val * j)] # and the max peaks vector

      if ( smooth_type[1] == "num.flaps" ){
        rmeanDis = c(rmeanDis, runningmean(obj, smth_type = "num.flaps" , 15, max.loop, split.val = split.val , j = j)) # use running mean function, and combine to previous splits
      } else {
        rmeanDis = c(rmeanDis,runningmean(obj, smth_type = "standard" , 401, max.loop))
      }
      #   print(paste ( j , "/" , times))
    }
  } else { # if the data do not need to be split.

    if ( smooth_type[1] == "num.flaps" ){
      rmeanDis = runningmean(displacement_rmean$z, smth_type = "num.flaps" , 15, max ) # Do the same
    } else {
      rmeanDis = runningmean(displacement_rmean$z, smth_type = "standard" , 401, max )
    }
  }

  # plot if test

  if(test){
    plot(displacement_rmean[,1])
    lines(rmeanDis, col = "red")

  }

  # remove running mean displacement

  displacement_rmean_removed = displacement_rmean$z - rmeanDis[1:nrow(displacement_rmean)]

  # plot if test

  if(test){

    plot(displacement_rmean_removed[,1], type = "l")
  }
  ##########################

  ## BUTTERWORTH FILTER

  ##########################

  but = butter( 4, 2.5/(sampling_freq/2),'high')

  z_but = rep(NA, nrow(acc_data))

  only = !is.na(displacement_rmean_removed) # only non NA

  z_but[only] = filtfilt(but, displacement_rmean_removed[only])

  data_but = data.frame(z = z_but) # and into data frame

  # plot if test

  if ( test){
    plot(data_but[,1], type = "l")
  }

  # Shift????

  amp_z_rmean = rep(NA, length(max))

  addn = 11 # ?????
  start= 8
  fin = length(max) - 6

  for ( i in start:fin) {

    shift = round((200/ff[i])/1/8)

    amp_z_rmean[i] = max(data_but[,1][max[i]:(max[i+1] +addn)] ) -
      min(data_but[,1][(max[i] - shift):(max[i+1] - shift)])
  }

  ############

  # INTERPOLATE

  ############

  int = interp1(1:length(maxtab), maxtab, 1:length(maxtab))
  p_p_z = int-mintab[1:length(int)]

  ############

  # RETURN DATA

  ############

  if( length( ff) != length(p_p_z)){
    ff = c(ff,NA)
  }

  ret = cbind( amp = amp_z_rmean*1000, p_p_z, ff, time = Time[max])

  return( ret)

}

