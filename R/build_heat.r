build_heat = function (leftright.res.cent, frontback.res.cent,i , cuts, uber_smoothness = 100){

  rows = floor( nrow(data)/cuts)

  beg = cbind (leftright.res.cent[((rows * (i-1))+1):(rows*i) ,1],
               frontback.res.cent[((rows * (i-1))+1):(rows*i) ,1])

  for ( j in 2:length(pigeon)){

    foo = cbind( leftright.res.cent[((rows * (i-1))+1):(rows*i) ,j],
                 frontback.res.cent[((rows * (i-1))+1):(rows*i) ,j])

    beg = rbind(beg,foo)
  }

  beg = as.data.frame(beg)
  names(beg) = c("x","y")

  return( ggplot(beg, aes(x, y))  +
            stat_density2d(geom="tile",
                           aes(fill=..density..),
                           #alpha=sqrt(sqrt(..density..))),
                           contour=FALSE, n= uber_smoothness)  +
            ylim(-30,30)+xlim(-30,30)+
         #   scale_alpha(range = c(0.5, 1.0)) +
           scale_fill_gradientn(colours = rev(rainbow(10)[1:7]), trans="sqrt"))+
  theme(text=element_text(size=16,  family="serif"))
  #scale_fill_continuous(  low = "white", high = "blue" ))



}
