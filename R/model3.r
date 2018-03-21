model = function ( dat , acf.const, scale){

  # dat = frontback.res.cent
  # acf.const = 600


  if( length( dim(dat)) != 2) stop ( "dat must be matrix, variable vs individual")
  sam.len = dim(dat)[1]/acf.const

  sam = sample( 1:dim(dat)[1], sam.len)

  if ( !scale) {
    sp = data.frame( sp =  as.vector(dat[sam,] ),
                     time = rep(sam, 6)/5/60,
                     id =  as.factor(rep(1:6, each = length(sam))))
  } else {
    sp = data.frame( sp = scale ( as.vector(dat[sam,] )),
                     time = scale( rep(sam, 6)) ,
                     id =  as.factor(rep(1:6, each = length(sam))))
  }

  m1 = lm(sp$sp~sp$time)
  m2 = lmer(sp~time+(1|id),data=sp)
  m3 = lmer(sp~time+(1+time|id),data=sp)

  plot( sp ~ time , data = sp)
  min.aic = which.min( c(AIC(m1),AIC(m2),AIC(m3)))

  if ( min.aic == 1){
    fit = lm(sp$sp~sp$time)
    print( "simple linear model is best")
  } else {
    if( min.aic == 2){
      fit = lmer(sp~time+(1|id),data=sp)
      print( "Random intercepts only is best")
    } else {
      fit = lmer( sp~time + (1+time|id), data = sp)
      print( "Random intercepts and slopes is best")
    }

  }


  sjp.lmer(fit, type =  "fe")

  return( list( confint(fit), sp, fit))


}
