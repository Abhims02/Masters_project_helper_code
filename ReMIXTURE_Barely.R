#paper level analysis
Library(ReMIXTURE)
regions_info <- data.table::fread("b_region_table_bridge.csv")
my_used_matrix <- readRDS("final_barely_dist.RDS")


# Creating remixture object for the  b_bridge_data
re_object_b_b <- ReMIXTURE::ReMIXTURE$new(my_used_matrix, regions_info)

# Params
RmHumansRunFname <- "rmbarley_Obj.rds"

##Doing remixture
if(!file.exists(RmHumansRunFname)){
  re_object_b_b$run(
    iterations = 100,
    subsample_proportions = 0.8,
    h_cutoffs = seq(0.01, 0.1, l = 30)
  )
  saveRDS(re_object_b_b,RmHumansRunFname)
} else {
  re_object_b_b <- readRDS(RmHumansRunFname)
}

####ASK Tim about this
re_object_b_b$run(
  iterations = 100,
  subsample_proportions = c(0.8),
  h_cutoffs = seq(min(self$dm), max(self$dm), length.out = 10),
  diagnosticPlotMDSclusters = FALSE,
)

#Let us look at distance densities
re_object_b_b$plot_distance_densities(samePlot = T)


###Looking at the h_cutoff values
re_object_b_b$plot_heatmaps()

#h-optimisation cure
re_object_b_b$plot_h_optimisation()



#Prob run 9,10,12,13,37,38,39, 62,63
#list regions
re_object_b_b$region_table

#New plotting
re_object_b_b$plot_maps(
  focalRegion = "Southern_Europe", # omit to print all. Useful in conjuction with `par(mfrow=c(<nRows>,<nCols>))`
  run = 19,
  range_lon = c(-179,179), # to play with map range, omit for the full globe
  range_lat = c(-85,85),      # to play with map range, omit for the full globe
  width_lims = c(2,8), # size of the circles (smallest and largest) in lat/lon units
  alpha_lims = c(0.05,0.8), # range of alpha values of the region-connecting lines. The alpha just scales along with line thickness (i.e. overlapped diversity) to help visual discrimination when the differences are small.
  projection = winkelIII, # choose a map projection. Or write your own--a function recieving and returning a data.table with numeric columns `lon` and `lat`.
  curvature_matrix = matrix(abs(rnorm(nrow(re_object_b_b$region_table)**2))/2,nrow=nrow(re_object_b_b$region_table),dimnames = list(re_object_b_b$region_table$region,re_object_b_b$region_table$region)) # curve the lines to prevent them overlapping. This curves them randomly but you can give it whatever matrix you wish. Currently in radians (will be degrees in future), with -vs and +ve values curving left/right.
)
