per.min = function(flaps = arr[,3,1], time = as.integer(round(arr[,4,1])  ), total.bins = NULL){


  Time.min = (time - min(time) +1)/60

  if ( is.null(total.bins)){
    total.bins = round(max(Time.min))
  }

  sq = as.integer(seq( min(time) , max(time), length.out = total.bins ))


  store = vector()
  for ( i in 1:total.bins){
    store = c(store , ( which( abs( time  - sq[i]) == min ( abs( time  - sq[i])  )) [1]))
  }

  store = c(store, length(time))

  mean = vector()
  sd   = vector()

  for ( i in 1:(length(store)-1)){

    mean = c(mean, mean(flaps[store[i]:store[i+1]],na.rm = T))
    sd   = c(sd  ,   sd(flaps[store[i]:store[i+1]], na.rm= T))
  }

  plot(mean~ Time.min[store[1:length(mean)]])

  output(mean)
}
