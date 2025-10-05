eckertIV <- function(dtLL,precision=0.001,projColNames=c("x_EIV","y_EIV")){
  dt <- copy(dtLL)
  radLon <- dt$X %>% deg2rad()
  radLat <- dt$Y %>% deg2rad()
#initial prediction of theta  
th  <- radLat/2
  dTh <- rep(1.0+precision,length(radLat))
#This is where the precision is achieved  
while(any(dTh>precision)){
    dTh <- -( th + sin(th)*cos(th) + 2*sin(th) - (2+(pi/2))*sin(radLat) ) / ( 2*cos(th)*(1+cos(th)) )
    th <- th + dTh
  }
  lon_W3 <- (2 / ((pi*(4+pi))**(1/2)) ) * radLon * (1+cos(th))
  lat_W3 <- (2 * ((pi*(4+pi))**(1/2)) ) * sin(th)
  dt[,  c(projColNames):=.( lon_W3 , lat_W3 )  ][]
}

k <- eckertIV(sfw)

plot(x = NULL, y = NULL,
     xlim=range(k$x_EIV),ylim=range(k$y_EIV))
####Prepare to be amazed
lapply(unique(swf_cord_proj$guIdx),function(l){
    lines(k[guIdx==l]$x_EIV,k[guIdx==l]$y_EIV)
})
points(k$x_EIV,k$y_EIV,pch=20,cex=0.1,col="#00000033")
