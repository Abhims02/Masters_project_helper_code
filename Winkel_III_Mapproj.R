library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(data.table)
#sf_use_s2(FALSE)

#all coastline_data in sf value format
#sfw <- rnaturalearthhires::coastline10
sfw_1 <- as.data.table(rnaturalearthhires::countries10)
#sfw[GEOUNIT!=SOVEREIGNT,GEOUNIT]
sfw <- as.data.table(sf::st_coordinates(rnaturalearthhires::countries10$geometry))
sfw[,guIdx:=.GRP,by=.(L1,L2,L3)]
sfw[,Reg_names:=sfw_1$NAME_LONG]
#sfw[,col:=applyPalette(guIdx,palettePresets$redbull)]


#converts it into coordinates
# sfw_cord <- as.data.frame(st_coordinates(sfw$geometry))
# #sfafc <- st_centroid(sfaf)
# 
# #Try adding the countries to the points
# test <- sfw_cord %>%
#   sf::st_as_sf(coords = c("X","Y"), crs = 4326)
# 
# 
# ###please work
magic <- rnaturalearthhires::countries10 %>%
  dplyr::select(NAME_LONG) %>% # keep only country name
  sf::st_transform(crs = 4326) %>%
  st_make_valid()
# 
# # within_sf <- sf::st_join(x = test, 
# #                          y = magic,
# #                          join = sf::st_within)
# 
# nearest_sf <- sf::st_join(x = test, 
#                           y = magic,
#                           join = sf::st_nearest_feature)
# 
# 
# ###Write this as an RDS file in remixture
# sfw_cord <- data.table(Long = sfw_cord$X, Lat = sfw_cord$Y, 
#                        Country = nearest_sf$NAME_LONG)
# 
# ##one way of plotting map lines using ggplot
# #"https://conservancy.umn.edu/bitstream/handle/11299/220339/time-maps-tutorial-v2.html?sequence=3&isAllowed=y"
# 
# indo <- sfw_cord %>%
#   filter(Country == "India")
# 
# plot(x = NULL, y = NULL,
#      xlim = c(min(indo$Long),max(indo$Long)),
#      ylim = c(min(indo$Lat),max(indo$Lat)),asp = 1)
# points(x = indo$Long, y = indo$Lat)
# #points(ld$lon,ld$lat,pch=20,cex=0.1,col="#00000033")

ld <- expand.grid(lon=seq(from=-180,to=180,by=5),lat=seq(from=-90,to=90,by=5))

#plotting the co-ordinates
plot(x = NULL, y = NULL,
     xlim=range(sfw$X),ylim=range(sfw$Y),asp = 1)
lapply(unique(sfw$guIdx),function(l){
  lines(sfw[guIdx==l]$X,sfw[guIdx==l]$Y)
})
#points(x = sfw_cord$Long, y = sfw_cord$Lat)
points(ld$lon,ld$lat,pch=20,cex=0.1,col="#00000033")

# plot(NULL,NULL,xlim=range(d$X),ylim=range(d$Y))
# lapply(unique(d$guIdx),function(l){
#   lines(d[guIdx==l]$X,d[guIdx==l]$Y,col=d[guIdx==l])
# })
########helper functions
deg2rad <- function(d = 90){
  r <- round((d * pi)/180,3)
  return(r)
}

rad2deg <- function(r = pi){
  d <- round((180 * r/pi),3)
  return(d)
}

####### Winkel-triple projection
winkel_triple <- function(df){ #df generated using sf value format (st_cord)
  ## in the df the 1st col is X(long) --lamda and the 2nd col is (lat) --phi
  
  #phi_1 is the standard parallel for the equirectangular projection
  phi_1 <- acos(2/pi)
  
  #alpha is the unnormalised cardinal sine function of phi and lambda
  alpha <- acos(cos(deg2rad(df[,2]))*cos(deg2rad(df[,1])/2))
  
  
  #Now finding the proj_x using the formula
  proj_x <- (1/2)*((deg2rad(df[,1])*cos(phi_1)) + (2*cos(deg2rad(df[,2]))*sin(deg2rad(df[,1])/2))/(sin(alpha)/alpha))
  
  #Now finding the proj_y using the formula
  proj_y <- (1/2)*(deg2rad(df[,2]) + (sin(deg2rad(df[,2]))/(sin(alpha)/alpha)))
  
  #values to return
  ret_df <- data.frame(Long = rad2deg(proj_x), Lat = rad2deg(proj_y))
  
  return(ret_df)
}

##using the function
swf_cord <- winkel_triple(sfw)
swf_cord_proj <- data.table(cbind(swf_cord, guIdx = sfw$guIdx))
elpd <- winkel_triple(ld)

# ## testing rotation
# plot(x = NULL, y = NULL,
#      xlim = c(min(sfw_cord$X),max(sfw_cord$X)),
#      ylim = c(min(sfw_cord$Y),max(sfw_cord$Y)),asp = 1)
# points(x = swf_cord_proj_test$X, y = swf_cord_proj_test$Y)



plot(x = NULL, y = NULL,
     xlim=range(sfw$X),ylim=range(sfw$Y),asp = 1)
####Prepare to be amazed
lapply(unique(swf_cord_proj$guIdx),function(l){
  lines(swf_cord_proj[guIdx==l]$X,swf_cord_proj[guIdx==l]$Y)
})
#points(x = swf_cord_proj[,1], y = swf_cord_proj[,2])
points(elpd$Long,elpd$Lat,pch=20,cex=0.1,col="#00000033")
#points(x = unlist(h1), y = unlist(h2))

##Testing our beautiful lines
x1 <- -105.2551
x2 <- -55.4915
y1 <- 54.5260
y2 <- -8.7832


k <- legendary_arc(x1,y1,x2,y2, r = 2, arc = -50)
k1 <- winkel_triple(k)
points(x = k1[,1], y = k1[,2])
