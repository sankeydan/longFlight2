
model = function ( dat, subsample = NULL, plot = T ){

  # Optional load

  # dat = dba.coh[,]
  # subsample = NULL

  # libraries

  library(mgcv)
  library(nlme)
  library(lme4)

  # Defensive coding

  if( length( dim(dat)) != 2) stop ( "dat must be matrix, variable vs individual")

  # subsample

  if(  is.null(subsample)){
    subsample = nrow(dat)
  }

  dat = dat[round(seq(1,nrow(dat),length.out = subsample)),]

  # scaled and unscaled dataframes

  dat1 = data.frame( sp =  as.vector(dat ),
                     time = rep(c(1:nrow(dat))/5/60, 6),
                     pid =  as.factor(rep(1:6, each = nrow(dat))))

  dat2 = data.frame( sp = scale ( as.vector(dat)),
                     time = scale( rep(c(1:nrow(dat))/5/60, 6)) ,
                     pid =  as.factor(rep(1:6, each = nrow(dat))))

  # Table to store the AIC vals

  AIC.table = data.frame( model = c("simple lm" ,
                                    "random intercepts",
                                    "corr+intercepts",
                                    "random slopes",
                                    "corr+slopes"),
                          scaled = rep(NA,5),
                          not.scaled = rep(NA,5))

  # Function for models

  modelling = function(dat){

    # dat = dat2

    # Remove NAs
    if( length (which(is.na(dat$sp)) != 0)){
      dat =  dat[ - which(is.na(dat$sp)),]
    }

    # unlist
    dat = data.frame(pid = unlist(dat$pid),
                     sp = unlist(dat$sp),
                     time=unlist(dat$time))

    #models
    m1 = lm(dat$sp~dat$time)
    m2 = lme(sp~time,  random= ~ 1|pid     ,data = dat)
    m3 = lme(sp~time,  random= ~ 1|pid     ,data = dat, correlation = corAR1(form = ~ 1|pid))
    m4 = try(lme(sp~time,  random= ~ 1+time|pid,data = dat))
    m5 = try(lme(sp~time,  random= ~ 1+time|pid,data = dat,correlation = corAR1(form = ~ 1+time|pid)))

    # # #work
    m6 = lmer(sp~time+ ( 1+time|pid),data = dat, correlation = corAR1())
    ACF.lme(m6)

    if( class(m4) == "try-error" & class(m5) == "try-error"){
      AICs = c(AIC(m1),AIC(m2),AIC(m3),NA,NA)
    }
    if( class(m4) == "try-error" & class(m5) != "try-error"){
      AICs = c(AIC(m1),AIC(m2),AIC(m3),NA,AIC(m5))
    }
    if( class(m4) != "try-error" & class(m5) == "try-error"){
      AICs = c(AIC(m1),AIC(m2),AIC(m3),AIC(m4), NA)
    }
    if( class(m4) != "try-error" & class(m5) != "try-error"){
      AICs = c(AIC(m1),AIC(m2),AIC(m3),AIC(m4),AIC(m5))
    }

    fit = get( paste0("m", which.min(AICs)))

    return( list(AICs, fit))
  }

  # Use function
  mod = modelling(dat1)
  mod.sca = modelling(dat2)


  AIC.table$not.scaled = mod[[1]]
  AIC.table$scaled     = mod.sca[[1]]

  fit = mod[[2]]
  fit.sca = mod.sca[[2]]

  # plot

  if ( plot){
    plot(dat1$sp~dat1$time, col = dat1$pid)
  }

  return( list( fit, fit.sca , AIC.table))


}
