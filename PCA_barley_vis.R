library(data.table)
library(ggplot2)
####Reading the file in
pca <- read.table("plink.eigenvec",header = F)
plot(x = pca$V3, y = pca$V4)


###The point is to plot it with region so that we know what is important
setwd("/data/gpfs/projects/punim1869/users/amadhusudans/Trial_genomes/filt_Trial_output/barley_remixture")

##read the passport data in
pop_data <- fread("211201_Milner2019_passport_data.csv")

pca_trim <- pca[,2:4]
colnames(pca_trim) <- c("Sample", "PC_1", "PC_2")

bri_reg_val <- character(length = length(pca_trim$Sample))
pos <- 1
for (name in pca_trim$Sample){
  samp_pos <- which(pop_data$sample.id == name)
  bri_reg <- pop_data$bridge_region[samp_pos]
  if(bri_reg == ""){
    bri_reg_val[pos] = NA
  }else{
    bri_reg_val[pos] = bri_reg
  }
  pos <- pos + 1
  
}

##Now adding it to pca_trim
pca_add <- cbind(pca_trim, bri_reg_val)
pca_final <- na.omit(pca_add)

ggplot(data=pca_final, aes(PC_1,PC_2, color = bri_reg_val)) + geom_point()


##########Plotting other PCs
pca_trim <- pca[,2:8]
colnames(pca_trim) <- c("Sample","PC_1", "PC_2","PC_3","PC_4",)

bri_reg_val <- character(length = length(pca_trim$Sample))
pos <- 1
for (name in pca_trim$Sample){
  samp_pos <- which(pop_data$sample.id == name)
  bri_reg <- pop_data$bridge_region[samp_pos]
  if(bri_reg == ""){
    bri_reg_val[pos] = NA
  }else{
    bri_reg_val[pos] = bri_reg
  }
  pos <- pos + 1
  
}

##Now adding it to pca_trim
pca_add <- cbind(pca_trim, bri_reg_val)
pca_final <- na.omit(pca_add)

ggplot(data=pca_final, aes(PC_2,PC_4, color = bri_reg_val)) + geom_point()


##Testing something
###sample df
df <- data.frame(x = c(1,2,3,4, 3,5,9),
                 y = c(7,11,NA,15,NA,34,56))

## This is for all the PCA work for progess and hopefully manuscript
library(rgl)
require(data.table)
require(BioDT)
#######################################################
## We do it for barley
setwd("/data/gpfs/projects/punim1869/users/amadhusudans/Trial_genomes/filt_Trial_output/barley_remixture/barley_pca")

#Reading the file
pca_barley <- fread("plink.eigenvec")

setwd("/data/gpfs/projects/punim1869/users/amadhusudans/Trial_genomes/filt_Trial_output/barley_remixture")

stuff <- data.table::fread("211201_Milner2019_passport_data.csv")


b_samp <- pca_barley$V2

#Loop to get a list of where the samples are from
b_reg <- character(length(b_samp))

pos <- 1
for(per in b_samp){
  samp_pos <- which(per == stuff$sample.id)
  pop_val <- stuff$bridge_region[samp_pos]
  if(pop_val == ""){
    b_reg[pos] <- NA
  } else {
    b_reg[pos] <- pop_val
  }
  pos <- pos + 1
}


#Now we add the 
pca_barley[,region := b_reg]

# Removing 
pca_barley_clean <- na.omit(pca_barley)

#need to manually add colours
library(RColorBrewer)

# Define a color palette using RColorBrewer
palette <- brewer.pal(n = length(unique(pca_barley_clean$region)), name = "Spectral")  # Adjust the number as needed
color_palette <- setNames(palette, unique(pca_barley_clean$region))  # Name the colors

# Add a color column based on the category column
pca_barley_clean[, color := color_palette[region]]


#Now we visualise
library(ggplot2)

ggplot(pca_barley_clean, aes(x = V3, y = V4, colour = region)) +
  geom_point() +
  scale_color_manual(values = color_palette) + 
  labs(x = "PC1 (16.62%)",
       y = "PC2 (14.40%)") +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),  # white plot area
    plot.background = element_rect(fill = "white", colour = NA),   # white outside
    panel.grid.major = element_blank(),  # no major gridlines
    panel.grid.minor = element_blank(),  # no minor gridlines
    axis.line = element_line(color = "black"),  # show axis lines
    axis.ticks = element_line(color = "black"), # keep axis ticks
    legend.background = element_rect(fill = "white", colour = "black"),
    legend.key = element_rect(fill = "white"))

#Check the variance that the PCs capture
var_barley <- fread("plink.eigenval")

plot(x = 1:nrow(var_barley), y = var_barley$V1, type = "o", xlab = "PCs", ylab = "Variance", 
     main = "Scree Plot for Variance captured by PCs")


# Now trying 3D plot for humans
# Open a 3D plotting window
options(rgl.printRglwidget = TRUE)

# Plot the PCA results
clear3d()

plot3d(
  pca_barley_clean[, 3:5],
  col = pca_barley_clean$color,  # Color by region
  size = 4,
  xlab = "PC1 (16.62%)",
  ylab = "PC2 (14.40%)",
  zlab = "PC3 (12.07%)"
)

################Mapdata test on the PCA
library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

region_list <- list(
  North_America = c("USA", "Canada"),
  South_America = c("Peru", "Chile", "Mexico", "Uruguay", "Brazil",
                    "Ecuador", "Bolivia", "Colombia", "Costa Rica",
                    "Guatemala", "Argentina", "El Salvador"),
  Oceania = c("Australia", "New Zealand"),
  Northern_Europe = c("France", "Germany", "UK",
                      "Netherlands", "Austria", "Hungary", "Poland",
                      "Belgium", "Switzerland", "Yugoslavia", "Croatia",
                      "Romania", "Russia", "Denmark", "Czechia",
                      "Ireland", "Sweden", "Finland", "Ukraine", "Belarus" ,
                      "Lithuania", "Norway", "Latvia", "Bulgaria",
                      "Albania", 'Czechoslovakia', "Estonia", "Moldova",
                      "Slovakia", "North Macedonia"),
  Southern_Europe = c("Italy", "Greece", "Spain",
                      "Portugal"),
  Central_Asia = c("Afghanistan", "Kazakhstan", "Uzbekistan",
                   "Kyrgyzstan", "Tajikistan", "Turkmenistan",
                   "Pakistan", "Bhutan", "India", "Nepal"),
  Far_East = c("China", "Japan", "South Korea", "North Korea",
               "Mongolia"),
  Near_East = c("Jordan", "Lebanon", "Armenia", "Azerbaijan",
                "Cyprus", "Syria", "Turkey", "Gorgia", "Iran", "Iraq",
                "Israel"),
  North_Africa = c("Libya", "Morocco", "Algeria", "Egypt", "Sudan",
                   "Tunisia", "Chad"),
  Arabian_Peninsula = c("Oman", "Saudi Arabia", "United Arab Emirates",
                        "Yemen"),
  Ethiopia = c("Ethiopia", "Eritrea")
)

region_df <- do.call(rbind, lapply(names(region_list), function(region_name) {
  data.frame(
    country = region_list[[region_name]],
    region_group = region_name,
    stringsAsFactors = FALSE
  )
}))

world_map <- map_data("worldHires")  # High-res map

map_colored <- world_map %>%
  left_join(region_df, by = c("region" = "country"))

ggplot(map_colored, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = region_group), color = "black", size = 0.2) +
  scale_fill_manual(
    values = c(
      "North_America" = "#9E0142",
      "South_America" = "#D53E4F",
      "Oceania" = "#ABDDA4",
      "Northern_Europe" = "#FDAE61",
      "Southern_Europe" = "#E6F598",
      "Central_Asia" = "#FEE08B",
      "Far_East" = "#FFFFBF",
      "Near_East" = "#F46D43",
      "North_Africa" = "#3288BD",
      "Arabian_Peninsula" = "#5E4FA2",
      "Ethiopia" = "#66C2A5"
    ),
    na.value = "lightgray",
    name = "Region"
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white")
  )




world_map <- map_data("worldHires")  # High-res map
#unique(world_map$region)

all_countries <- unlist(region_list)

# Filter the world map data
subset_map <- world_map[world_map$region %in% all_countries, ]


ggplot(subset_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  coord_fixed(1.3) +
  theme_minimal()
