library(dplyr)
library(data.table)
#####Trying everything works in the plotter
##Helper functions load them before running the ploter_update_101
rounded_line <- function(x1,y1,x2,y2,r,return_mat=F,arc = 0, n = 200){
  theta <- angle(x1,y1,x2,y2)
  if(arc == 0){
    cir_1 <- circle_seg(0,0,radius=r/2,start_radians=pi*(1/2),end_radians=pi*(3/2),n=n)
    cir_2 <- cir_1 %>% rotate(pi) %>% translate(by_y = euc_dist(x1,x2,y1,y2))
    mid1 <- filled_line(last(t(cir_1))[,1],last(t(cir_1))[,2],first(t(cir_2))[,1] , first(t(cir_2))[,2], n=n, ends = F )
    mid2 <- filled_line(last(t(cir_2))[,1],last(t(cir_2))[,2],first(t(cir_1))[,1] , first(t(cir_1))[,2], n=n, ends = F )
    
    
    # #circle at origin -- only a part of it
    # cir_1 <- circle_seg(0,0,radius = r/2, start_radians =  pi - deg2rad(arc),
    #                     end_radians = 2*pi- deg2rad(arc), n=n)
    # 
    # #circle at (0,1) -- only a part of it
    # cir_2 <- cir_1 %>% rotate(pi) %>% translate(by_y = euc_dist(x1,x2,y1,y2))
    # 
    # #constructing the straight lines
    # mid1 <- filled_line(last(t(cir_1))[,1],last(t(cir_1))[,2],first(t(cir_2))[,1] , first(t(cir_2))[,2], n=n, ends = F )
    # 
    # mid2 <- filled_line(last(t(cir_2))[,1],last(t(cir_2))[,2],first(t(cir_1))[,1] , first(t(cir_1))[,2], n=n, ends = F )
    # 
    #combine to a matrix
    shape <- cbind(
      cir_1,
      mid1,
      cir_2,
      mid2
    ) %>%
      rotate(theta) %>%
      translate(x1,y1)
    
    if(return_mat==TRUE){
      shape
    } else {
      data.table(
        x=shape[1,],
        y=shape[2,]
      )
    }
  } else if(arc > 0){
    #circle at origin -- only a part of it
    cir_1 <- circle_seg(x1,y1,radius = r/2, start_radians =  pi - deg2rad(arc),
                        end_radians = 2*pi- deg2rad(arc), n=n)
    
    #circle at (0,1) -- only a part of it
    cir_2 <- circle_seg(x2,y2,radius = r/2, start_radians = 2*pi + deg2rad(arc),
                        end_radians = pi+deg2rad(arc), n=n)
    #for the outer circle
    mid1 <- circle_seg(x = euc_dist(x1,x2,y1,y2)/2,
                       y = -((euc_dist(x1,x2,y1,y2)/2)/tan(deg2rad(arc))), 
                       start_radians = 2*pi - deg2rad(arc),
                       end_radians = deg2rad(arc),
                       radius = (euc_dist(x1,x2,y1,y2)/2)/sin(deg2rad(arc)) + r/2, n=n)
    
    #for the inner circle
    mid2 <- circle_seg(x = euc_dist(x1,x2,y1,y2)/2,
                       y = -((euc_dist(x1,x2,y1,y2)/2)/tan(deg2rad(arc))),
                       start_radians = 2*pi - deg2rad(arc),
                       end_radians = deg2rad(arc) + 2*pi,
                       radius = ((euc_dist(x1,x2,y1,y2)/2)/sin(deg2rad(arc))) - r/2, n=n)
    
    #reversing the inner_circle
    #mid2 <- rev_mat(inner_cir)
    
    #combine to a matrix
    shape <- cbind(
      cir_1,
      mid1,
      cir_2,
      mid2
     ) #%>%
    #   rotate(theta) %>%
    #   translate(x1,y1)
    
    if(return_mat==TRUE){
      shape
    } else {
      data.table(
        x=shape[1,],
        y=shape[2,]
      )
    }
  } else if(arc < 0){
    #circle at origin -- only a part of it
    cir_1 <- circle_seg(x1,y1,radius = r/2, start_radians =  pi - deg2rad(arc),
                        end_radians = 2*pi- deg2rad(arc), n=n)
    
    #circle at (0,1) -- only a part of it
    cir_2 <- circle_seg(x2,y2,radius = r/2, start_radians = 2*pi + deg2rad(arc),
                        end_radians = pi+deg2rad(arc))
    #for the outer circle
    mid1 <- circle_seg(x = euc_dist(x1,x2,y1,y2)/2,
                       y = -((euc_dist(x1,x2,y1,y2)/2)/tan(deg2rad(arc))),
                       start_radians = -(2*pi - deg2rad(arc)),
                       end_radians = -(deg2rad(arc) + 2*pi),
                       radius = ((euc_dist(x1,x2,y1,y2)/2)/sin(deg2rad(arc))) - r/2, n=n)
    
    #for the inner circle
    mid2 <- circle_seg(x = euc_dist(x1,x2,y1,y2)/2,
                       y = -((euc_dist(x1,x2,y1,y2)/2)/tan(deg2rad(arc))), 
                       start_radians = -(2*pi - deg2rad(arc)),
                       end_radians = -(deg2rad(arc)),
                       radius = (euc_dist(x1,x2,y1,y2)/2)/sin(deg2rad(arc)) + r/2, n=n)
    
    #reversing the inner_circle
    #mid2 <- rev_mat(inner_cir)
    
    #We need a final matrix of all the points for ReMIXTURE to plot it
    ## set in a perticular order
    #final_mat <- cbind(cir_1,outer_cir,cir_2,inner_cir_final)
    #combine to a matrix
    shape <- cbind(
      cir_1,
      mid1,
      cir_2,
      mid2
    ) #%>%
      # rotate(theta) %>%
      # translate(x1,y1)
      
    if(return_mat==TRUE){
      shape
    } else {
      data.table(
        x=shape[1,],
        y=shape[2,]
      )
    }
  }
}

angle <- function(x1,y1,x2,y2){
  x <- x2 - x1
  y <- y2 - y1
  l <- euc_dist(x1,x2,y1,y2)
  ifelse(x<=0,2*pi - acos(y/l),acos(y/l))
}

filled_line <- function(x1,y1,x2,y2,n=200,ends=T){
  m <- matrix(
    c(seq(x1,x2,length.out=n),
      seq(y1,y2,length.out=n)),
    nrow=2,byrow=T
  )
  if(ends==T){
    m
  } else {
    m[,2:(ncol(m)-1)]
  }
}

translate <- function(m,by_x=0,by_y=0){
  T_ <- matrix(
    c(
      1,0,by_x,
      0,1,by_y,
      0,0,1
    ),nrow=3,byrow=T
  )
  T_ %*% rbind(m,1) %>% `[`(1:2,)
}

rotate <- function(m,theta=pi){
  R_ <- matrix(
    c(
      cos(-theta) , -sin(-theta),
      sin(-theta)          , cos(-theta)
    ),nrow=2,byrow=T
  )
  R_ %*% m
}

euc_dist<-function(x1,x2,y1,y2){
  sqrt( ((x1-x2)**2) + ((y1-y2)**2) )
}


deg2rad <- function(d = 90){
  r <- round((d * pi)/180,3)
  return(r)
}
##############################
circle_seg <- function(
    x=0,
    y=0,
    radius=1,
    start_radians=pi,
    end_radians=pi,
    n=200
){
  start_radians <- start_radians %% (2*pi)
  end_radians <- end_radians %% (2*pi)
  if(start_radians!=end_radians){ #not a full circle
    if(start_radians > end_radians){
      n <- round( n*((2*pi)+end_radians-start_radians)/(2*pi))
      s <- seq(start_radians,(2*pi)+end_radians,length.out=n) %% (2*pi)
    } else {
      n <- n * ((end_radians-start_radians)/(2*pi))
      s <- seq(start_radians,end_radians,length.out=n) %% (2*pi)
    }
  } else { #full circle
    n <- round(n)+1
    s <- seq(start_radians,start_radians+(2*pi),length.out=n) %% (2*pi)
  }
  matrix(
    c(x+radius*sin(s),y+radius*cos(s)),
    ncol=length(s),
    byrow=T
  )
}
##################################
###(WARNING....)) This can only be used in this func -- BEWARE!!!!!!
rev_mat <- function(m){
  rev_row_1 <- rev(m[1,])
  rev_row_2 <- rev(m[2,])
  df <- data.frame(rev(m[1,]), 
                   rev(m[2,]))
  #re_mat <- matrix(c(rev_row_1, rev_row_2), nrow = 2, byrow = F)
  return(t(df))
}

#######################################
# ######plotter update 101 (leg_theta)
# leg_theta <- function(theta = 0, #default is 0 (deg)
#                       r = 0.2){ #default pivotal circle radius
#   
#   #If theta is 0 then do this base case straight lines
#   if(theta == 0){
#     #circle at origin -- only a part of it
#     cir_1 <- circle_seg(0,0,radius = r, start_radians =  pi - deg2rad(theta),
#                         end_radians = 2*pi- deg2rad(theta))
#     
#     #circle at (0,1) -- only a part of it
#     cir_2 <- circle_seg(1,0,radius = r, start_radians = 2*pi + deg2rad(theta),
#                         end_radians = pi+deg2rad(theta))
#     
#     #constructing the straight lines
#     #x-axis remains the same (0,1), the y-axis needs offset
#     y_line <- rep(0, times = 1000)
#     x_line <- seq(0,1, length.out = 1000)
#     
#     #offsetting for the 2 different conditions
#     y_line_1 <- y_line + r #top line 
#     y_line_2 <- y_line - r #bottom line 
#     
#     #points in mat format for top line
#     mat_1 <- matrix(c(x_line,y_line_1), ncol = length(x_line),byrow = T)
#     
#     #points in mat format for bottom line -- (rev of bottom line)
#     mat_2 <- matrix(c(x_line,rev(y_line_2)), ncol = length(x_line),byrow = T)
#     
#     #We need a final matrix of all the points for ReMIXTURE to plot it
#     ## set in a perticular order
#     final_mat <- cbind(cir_1,mat_1,cir_2,mat_2)
#     
#     return(final_mat)
#     
#   }else if(theta > 0){
#     #circle at origin -- only a part of it
#     cir_1 <- circle_seg(0,0,radius = r, start_radians =  pi - deg2rad(theta),
#                         end_radians = 2*pi- deg2rad(theta))
#     
#     #circle at (0,1) -- only a part of it
#     cir_2 <- circle_seg(1,0,radius = r, start_radians = 2*pi + deg2rad(theta),
#                         end_radians = pi+deg2rad(theta))
#     
#     #for the outer circle
#     outer_cir <- circle_seg(x = 0.5,
#                             y = -(0.5/tan(deg2rad(theta))), 
#                             start_radians = 2*pi - deg2rad(theta),
#                             end_radians = deg2rad(theta),
#                             radius = 0.5/sin(deg2rad(theta)) + r)
#     
#     #for the inner circle
#     inner_cir <- circle_seg(x = 0.5,
#                             y = -(0.5/tan(deg2rad(theta))),
#                             start_radians = 2*pi - deg2rad(theta),
#                             end_radians = deg2rad(theta) + 2*pi,
#                             radius = (0.5/sin(deg2rad(theta))) - r)
#     
#     #reversing the inner_circle
#     inner_cir_final <- rev_mat(inner_cir)
#     
#     #We need a final matrix of all the points for ReMIXTURE to plot it
#     ## set in a perticular order
#     final_mat <- cbind(cir_1,outer_cir,cir_2,inner_cir_final)
#     
#     return(final_mat)
#     
#   }else if(theta < 0){
#     #circle at origin -- only a part of it
#     cir_1 <- circle_seg(0,0,radius = r, start_radians =  pi - deg2rad(theta),
#                         end_radians = 2*pi- deg2rad(theta))
#     
#     #circle at (0,1) -- only a part of it
#     cir_2 <- circle_seg(1,0,radius = r, start_radians = 2*pi + deg2rad(theta),
#                         end_radians = pi+deg2rad(theta))
#     
#     #for the outer circle
#     outer_cir <- circle_seg(x = 0.5,
#                             y = -(0.5/tan(deg2rad(theta))),
#                             start_radians = -(2*pi - deg2rad(theta)),
#                             end_radians = -(deg2rad(theta) + 2*pi),
#                             radius = (0.5/sin(deg2rad(theta))) - r)
#     
#     #for the inner circle
#     inner_cir <- circle_seg(x = 0.5,
#                             y = -(0.5/tan(deg2rad(theta))), 
#                             start_radians = -(2*pi - deg2rad(theta)),
#                             end_radians = -(deg2rad(theta)),
#                             radius = 0.5/sin(deg2rad(theta)) + r)
#     
#     #reversing the inner_circle
#     inner_cir_final <- rev_mat(inner_cir)
#     
#     #We need a final matrix of all the points for ReMIXTURE to plot it
#     ## set in a perticular order
#     final_mat <- cbind(cir_1,outer_cir,cir_2,inner_cir_final)
#     
#     return(final_mat)
#   }
# }
# ######################################
# #Testing the function
# #theta positive
# pos_theta <- leg_theta(theta = 60)
# plot(x = NULL, y = NULL,
#      xlim = c(-2,2),ylim = c(-2,2),asp = 1,main="Test draw.arc")
# points(pos_theta[1,],pos_theta[2,])
# 
# #theta negative
# neg_theta <- leg_theta(theta = -60, r = 0.1)
# plot(x = NULL, y = NULL,
#      xlim = c(-2,2),ylim = c(-2,2),asp = 1,main="Test draw.arc")
# points(neg_theta[1,],neg_theta[2,])
# 
# 
# #theta 0
# nil_theta <- leg_theta(theta = 0, r = 0.5)
# plot(x = NULL, y = NULL,
#      xlim = c(-2,2),ylim = c(-2,2),asp = 1,main="Test draw.arc")
# points(nil_theta[1,],nil_theta[2,])
# 
# 
# plot(x = NULL, y = NULL,
#      xlim = c(-2,2),ylim = c(-2,2),asp = 1,main="Test draw.arc")
# points(leg_theta()[1,],leg_theta()[2,])
# 

##########################Testing something
x1 <- 0
x2 <- 1
y1 <- 0
y2 <- 1

h1 <-rounded_line(x1,y1,x2,y2, r = 2, arc = 40)[,1]
h2 <- rounded_line(x1,y1,x2,y2, r = 2, arc = 40)[,2]
plot(x = unlist(h1), y = unlist(h2))

# arc = 0
r= 0.2
# cir_1 <- circle_seg(0,0,radius = r/2, start_radians =  pi - deg2rad(arc),
#                     end_radians = 2*pi- deg2rad(arc))


arc = 40
theta <- angle(x1,y1,x2,y2)
plot(x = NULL, y = NULL,
     xlim = c(-2,2),ylim = c(-2,2),asp = 1,main="Test draw.arc")
cir_1 <- circle_seg(0,0,radius = r/2, start_radians =  pi - deg2rad(arc),
                    end_radians = 2*pi- deg2rad(arc), n=200) %>% rotate(-theta)

#circle at (0,1) -- only a part of it
cir_2 <- circle_seg(0,0,radius = r/2, start_radians = 2*pi + deg2rad(arc),
                    end_radians = pi+deg2rad(arc)) %>% 
  translate(by_y = euc_dist(x1,x2,y1,y2)) %>% rotate(theta)
shape <- cbind(
  cir_1,
  cir_2
) %>%
  rotate(theta) %>% 
  translate(x1,y1)
points(shape[1,], shape[2,])
points(cir_1[1,],cir_1[2,])
points(cir_2[1,],cir_2[2,])
