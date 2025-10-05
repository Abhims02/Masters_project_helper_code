Code for ADMIXTURE visualise barely
#ADMIXTURE (v1.3.0)
#mapmixture(v1.2.0)
## We need to get the passport data
setwd("/data/gpfs/projects/punim1869/users/amadhusudans/Trial_genomes/filt_Trial_output/barley_remixture")
barley_passport <- fread("211201_Milner2019_passport_data.csv")

##Admixture_for barley
setwd("/data/gpfs/projects/punim1869/users/amadhusudans/Trial_genomes/filt_Trial_output/barley_remixture/admixture_barley/admixture_k_val_bar")

#try plotting admixture
tbl=read.table("hapmap_subset.3.Q")
barplot(t(as.matrix(tbl)), col=rainbow(3), xlab="Individual #", ylab="Ancestry", border=NA)

# admixture with K val = 11 (11 regions)
tbl= fread("hapmap_subset.11.Q")

#We need to add the region information
setwd("/data/gpfs/projects/punim1869/users/amadhusudans/Trial_genomes/filt_Trial_output/barley_remixture/admixture_barley/sub_set_files")

samp_names <- fread("hapmap_subset.nosex")
samps_to_remove <- fread("hapmap_subset.irem")

#Removing the samples
samp_names <- samp_names[!V2 %in% samps_to_remove$V2]


# Adding sample names
tbl[,Sample := samp_names$V2]

#Loop to get the super population
sup_pop <- character(length = 22606L)
pos <- 1
for(per in tbl$Sample){
  samp_pos <- which(per == barley_passport$sample.id)
  pop_val <- barley_passport$bridge_region[samp_pos]
  if(pop_val == ""){
    sup_pop[pos] <- "no_reg"
  } else {
    sup_pop[pos] <- pop_val
  }
  pos <- pos + 1
}

#add the sup_pop
tbl[,Sample_region := sup_pop]


# Some samples have no_region we remove that
reg_filt_ances <- tbl[Sample_region != "no_reg"]

#Now adding color to the regions and ordering it
cols_to_add <- c("red", "blue", "green", "orange", "purple", "brown", "pink", 
                 "yellow", "cyan", "gray", "#008080")

# Create a named vector where names are regions and values are the corresponding colors
region_colors <- setNames(cols_to_add, unique(reg_filt_ances$Sample_region))

# Assign the corresponding color to each region in the data.table
reg_filt_ances[, color := region_colors[Sample_region]]

setorder(reg_filt_ances, Sample_region)

#Save this file
#saveRDS(reg_filt_ances, file = "barley_admix_k11.RDS")

reg_filt_ances <- readRDS("barley_admix_k11.RDS")
#table(reg_filt_ances[,13:14])


to_test <- t(as.matrix(reg_filt_ances[19149:20033,1:11]))

barplot(t(as.matrix(reg_filt_ances[,1:11])), col=unique(reg_filt_ances$color),
        xlab="Individuals", ylab="Ancestry", border=NA)

#manually adding abline
abline(v = 71.5, col="black", lwd=2)
abline(v = 2173.5, col="black", lwd=2)
abline(v = 7376.5, col="black", lwd=2)
abline(v = 9360.5, col="black", lwd=2)
abline(v = 12147.5, col="black", lwd=2)
abline(v = 12611.5, col="black", lwd=2)
abline(v = 13396.5, col="black", lwd=2)
abline(v = 18888.5, col="black", lwd=2)
abline(v = 18938.5, col="black", lwd=2)
abline(v = 19148.5, col="black", lwd=2)
#abline(v = 20033.5, col="black", lwd=2)

########Another method
barplot(t(as.matrix(reg_filt_ances[,1:11])), col=unique(reg_filt_ances$color),
        xlab="Individuals", ylab="Ancestry", border=NA, yaxt = 'n')

#manually adding abline
abline(v = 85.2, col="black", lwd=2)
abline(v = 2607.7, col="black", lwd=2)
abline(v = 8851.3, col="black", lwd=2)
abline(v = 11232.1, col="black", lwd=2)
abline(v = 14576.5, col="black", lwd=2)
abline(v = 15133.3, col="black", lwd=2)
abline(v = 16075.3, col="black", lwd=2)
abline(v = 22665.7, col="black", lwd=2)
abline(v = 22725.7, col="black", lwd=2)
abline(v = 22977.7, col="black", lwd=2)




p <- barplot(t(as.matrix(reg_filt_ances[,1:11])), col=unique(reg_filt_ances$color),
             xlab="Individual #", ylab="Ancestry", border=NA)



rdat <- data.table(
  x=p,
  colAncest=reg_filt_ances$color,
  y=rep(1,length(p)),
  pch=22
)


rdat[, points(
  x=x,
  y=y,
  col=colAncest,
  pch=pch,
  cex=3
) ]


###################################################
#Another way to look at the data To confirm we are doing it correctly\
library(mapmixture)

# Test data
file <- system.file("extdata", "admixture1.csv", package = "mapmixture")
admixture1 <- read.csv(file)


# Read in coordinates file
file <- system.file("extdata", "coordinates.csv", package = "mapmixture")
coordinates <- read.csv(file)

# Run mapmixture
map1 <- mapmixture(admixture1, coordinates, crs = 3035)
# map1


#Set our data (make sure it is region wise and not bridge region wise)
admix_data <- data.frame(Site = reg_filt_ances$Sample_region,
                         Ind = reg_filt_ances$Sample,
                         Cluster1 = reg_filt_ances$V1,
                         Cluster2 = reg_filt_ances$V2,
                         Cluster3 = reg_filt_ances$V3,
                         Cluster4 = reg_filt_ances$V4,
                         Cluster5 = reg_filt_ances$V5,
                         Cluster6 = reg_filt_ances$V6,
                         Cluster7 = reg_filt_ances$V7,
                         Cluster8 = reg_filt_ances$V8,
                         Cluster9 = reg_filt_ances$V9,
                         Cluster10 = reg_filt_ances$V10,
                         Cluster11 = reg_filt_ances$V11)


#save this as a .CSV file as well
write.csv(admix_data, file = "mapmix_form_data.csv", row.names = F)
admix_data <- read.csv("mapmix_form_data.csv")
#coordinated
admix_cord <- data.frame(Site = unique(admix_data$Site),
                         Lat = c(21.4735,
                                 20.5937,
                                 15.1794,
                                 35.8617,
                                 38.9637,
                                 26.3351,
                                 38.7946,
                                 61.9241,
                                 -25.2744,
                                 -14.2350,
                                 39.3999),
                         Lon = c(55.9754,
                                 78.9629,
                                 39.7823,
                                 104.1954,
                                 35.2433,
                                 17.2283,
                                 -106.5348,
                                 25.7482,
                                 133.7751,
                                 -51.9253,
                                 -8.2245))


write.csv(admix_cord, file = "mapmix_form_cord.csv", row.names = F)
admix_cord <- read.csv("mapmix_form_cord.csv")
#Let us try doing a map work!!!
map1 <- mapmixture(
  admixture_df = admix_data,
  coords_df = admix_cord,
  cluster_cols = unique(reg_filt_ances$color),
  cluster_names = NULL,
  crs = 4326,
  boundary = c(xmin=-179, xmax=179, ymin=-89, ymax=89),
  pie_size = 10,
  #pie_border = 0.2,
  pie_opacity = 1,
  land_colour = "#d9d9d9",
  sea_colour = "#deebf7",
  expand = FALSE,
  arrow = F,
  arrow_size = 1,
  arrow_position = "tl",
  scalebar = F,
  scalebar_size = 1,
  scalebar_position = "tl",
  #plot_title = "Mapmixture Figure",
  #plot_title_size = 15,
  #axis_title_size = 12,
  #axis_text_size = 10
) + 
  theme(legend.position = "none")
map1
