model2 = function ( dat, acf.const, scale = F, plot = F, conf.method = "std.er", specify.model = NULL){

  # Optional load

  # dat = ff.coh
  # acf.const = 100
  # scale = F
  
  # libraries

  library(lme4)
  library(mgcv)
  library(MASS)

  # Defensive coding

  if( length( dim(dat)) != 2) stop ( "dat must be matrix, variable vs individual")


  # data manipulation

  sam.len = nrow( dat)/acf.const
  sam = sample ( 1:nrow(dat), sam.len)

  if ( scale){
    dat = data.frame( sp =  scale(as.vector(dat[sam,] )),
                      time = scale(rep(sam/5/60, 6)),
                      pid =  as.factor(rep(1:6, each = length(sam))))
  } else {
    dat = data.frame( sp =  as.vector(dat[sam,] ),
                      time = rep(sam/5/60, 6),
                      pid =  as.factor(rep(1:6, each = length(sam))))
  }

  # plot?
  if ( plot){
    plot( dat$sp~ dat$time, col = as.numeric(dat$pid))
  }

  # models

  m1 = lm(dat$sp~dat$time)
  m2 = lmer(sp~time+(1|pid),data=dat)
  m3 = lmer(sp~time+(1+time|pid),data=dat)

  # model selection

  if ( is.null(specify.model)){
    AICs = c(AIC(m1),AIC(m2), AIC(m3))
    whi.mi = which.min(AICs)
    whi.mi
    fit = get( paste0("m",whi.mi))
   # print( c("simple linear model is best", "random intercepts only","random intercepts and slopes")[whi.mi])
  } else {
    fit = get( paste0( "m", specify.model))
  }

  # confidence intervals

  if( conf.method == "std.er"){
    sum = summary(fit)
    st. = sum$coefficients[2,"Std. Error"]*1.96
    es. = sum$coefficients[2,"Estimate"]
    CI = c(es. -st., es.+st. )
    CI
  }
  if( conf.method == "confint"){
    con = confint(fit)
    CI = con[which(names(con[,1]) == "time"),]
    CI
  }



  return( list(fit, CI, AICs))


}
