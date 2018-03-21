#' @export
#'

plot_and_stats = function (arr,  LMM.plot = T, qq.plot = F){

  library(plyr)
  library(nlme)
  library(lme4)
  library(ggplot2)

  # objects
  len = dim(arr)[1]

  # convert array into data frame and trim
  dat = adply(arr, 3)
  dat = as.matrix(dat[,c(1,4:5)])

  # add a group mean to the data
  dat = rbind(dat, cbind( rep(7, len),
                    rowMeans(arr[,3,]),
                    dat[1:len,3] ))

  # name dat
  dat = as.data.frame(dat)
  names(dat) = c("ind","ff","time")
  dat$ff = as.numeric(dat$ff)
  dat$ind= as.numeric(dat$ind)
  dat$time=as.numeric(dat$time)
  # PLOT

  if ( qq.plot ){
    par(mfrow = c(2,1))
  }

  tail(dat)

  # LMM
  m.mixed = lmer(ff ~ time + (1 | ind ), data = dat)
  coefs <- data.frame(coef(summary(m.mixed)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))

  rain = c(rainbow(6),"black")


    p = ggplot( dat, aes( x = time, y = ff)  )
    p = p +
      #geom_point(aes(color=as.factor(ind))) +
      geom_smooth(aes(color=rain[ind]),
                  method=lm, se=FALSE, fullrange=TRUE) +
      geom_abline(intercept = 0, slope = 0)
    print(p)


  # LM
  mass.mod = summary(lm(delta~ mass))
  tars.mod = summary(lm(delta~ tarsus))

  group2.mod = summary(lm(delta[g2]~ mass[g2]))
  group1.mod = summary(lm(delta[g1]~ mass[g1]))

  return ( list( list(paste("Linear model for only group1"), group1.mod),
                 list(paste("linear model for only group2"), group2.mod),
                 list( paste("Linear model, delta over mass"), mass.mod) ,
                 list(paste("Linear model, delta over tarsus length"), tars.mod) ,
                 list(paste("mixed model with group as random"), paste("P value = "), coefs$p.z[2], paste("summary "), summary(m.mixed))))


}
