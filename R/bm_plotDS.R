#' @export
#'
#' @params error Either SE (standard error), or SD (standard deviation)
#'

bm_plotDS = function( vec = NULL ,
                      matr = NULL,
                      x = NULL,
                      total.bins = 50,
                      error = "SE",
                      tick.size = 0.25,
                      ylim.scale = 2,
                      plot.error = T,
                      ylab = "y",
                      xlab = "x",
                      ylim = NULL,
                      return_means = T) {

  # vec = y
  #
  # matr = dba.coh
  # total.bins = 100
  # error = "SD"
  # tick.size = 0.25
  # ylim.scale = 2
  # plot.error = T
  # ylab = "y"
  # xlab = "x"
  # return_means = T
  # ylim = NULL
  # x = NULL

  if ( !is.null(matr)){

    # matr2 = matr[1:3, ]
    sq =  floor(seq( 0, nrow(matr), length.out =  total.bins))

    mean = rep ( NA, length(sq))
    sd =   rep ( NA, length (sq))

    len = sq[2]-sq[1]

    for ( i in 1:(length(sq)-1)){
      mean[i] = mean(as.vector(matr[(sq[i]+1):sq[i+1],]), na.rm = T)
      sd  [i] =   sd(as.vector(matr[(sq[i]+1):sq[i+1],]), na.rm = T)
    }


  } else {

    len =  floor(length(vec)/ total.bins)

    mat =  split (vec, ceiling(seq_along(vec)/len))

    mean = unlist(lapply( mat,mean, na.rm = T))

    mean = mean[1:total.bins]

    sd   = as.numeric(unlist(lapply( mat,sd , na.rm = T )))

    sd   = sd[1:total.bins]
  }


  if ( error == "SD" ){
    er = sd
  } else {
    er = sd/ sqrt(len)
  }


  tic = mean (er) * tick.size

  if( is.null(ylim)){
  if ( plot.error){
    ylim = c( (min(mean,  na.rm = T) - max(er, na.rm = T)*ylim.scale),
              (max(mean,  na.rm = T) + max(er, na.rm = T)*ylim.scale))

  }
  }

  if ( is.null(x)){
    x = c(1:length(mean))
  } else {
    x = seq(x[1],x[2],length.out = length(mean))
  }
  plot(mean ~ x ,
       ylim = ylim,
       xlab = xlab,
       ylab = ylab )



  if ( plot.error){

    for ( i in 1:length(sd)){
      segments( x[i], mean[i]- er[i], x[i], mean[i] + er[i])
    }
  }

  if ( return_means ){
    return(mean)
  }

}


