

# Clear workspace

rm(list= ls())



files = list.files(file.path(PROJHOME, "Output", "mGPS"))

smooth_type = "num.flaps"

load( file.path( PROJHOME, "Data" , "pigeon_nums.rda"))

library("stringr")

time.errors = vector()
no.file = vector()

for ( i in 1:length(files)){ # for each file in the mGPS folder

  # i = 6

  load( file.path ( PROJHOME , "Output" , "gGPS", "g2.34.rda"))

  fin.time = dim(data)[1]

  load( file.path ( PROJHOME , "Output" , "mGPS" , files[i])) # load the GPS data for start and end time
  start = data$UTC.TIME[1] # record start and end times

  fin = data$UTC.TIME[fin.time]

  f_id = str_split_fixed(files[i], "\\.", 17)[,11] # split the filename

  ACCfiles = list.files( file.path (PROJHOME , "Data","ACC"))

  a_id = str_split_fixed(ACCfiles, "\\.", 17)[,12]

  j = which(a_id == f_id)


  data = ff_dba(ACCfiles[j], start, fin, smooth_type = smooth_type) # use flap freq/ dorsal body acceleration function to

plot( data[,1])

  save(data , file= file.path( PROJHOME , "Output" , "mACC", paste0( substr(files[i], 1, nchar(files[i])- 4), ".", smooth_type, ".rda")))

  print(i)
}
