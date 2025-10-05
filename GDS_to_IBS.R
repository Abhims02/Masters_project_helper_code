#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("SeqArray")
BiocManager::install("SNPRelate")
setwd("/data/gpfs/projects/punim1869/transfer")


Morex_ibs_data <- SNPRelate::snpgdsIBS(morex_gds)

#The filters put on the samples according to Milner et.al,2019
# nSubsample <- 10e3L
# maxMissPos <- 0.1000
# minMAF <-     0.0005

## subsampling in gds
BridgeMorexV3snpGdsFname <- "210119_MorexV3_GBS_Milner2019.snp.filtered.gds"
gdsHandle <- SNPRelate::snpgdsOpen(BridgeMorexV3snpGdsFname)
selSNP <- SNPRelate::snpgdsSNPList(gdsHandle)[sample(nSubsample) %>% sort,"snp.id"]
SNPRelate::snpgdsGDS2PED(gdsHandle, snp.id = selSNP,
                         "barley_data_subset")




##Trying to get plink bed file from the gds file
Morex_bed_data <- SNPRelate::snpgdsGDS2BED(morex_gds,
                                           "barley_data")



my_used_matrix[1:10,1:10]

more_ibs_matrix <- Morex_ibs_data$ibs
saveRDS(more_ibs_matrix, file = "morex_ibs_matrix.rds")
#test <- readRDS("morex_ibs_matrix.rds")
#test[1:5,1:5]

#keepCols <- !is.na(colSums(test))
#keepRows <- !is.na(rowSums(test))

#test <- test[keepRows,keepCols]



#require(magrittr)
#(m<-matrix(c(1,2,3,NA),ncol=2)) %>% na.omit %>% str

writeLines(Morex_ibs_data$sample.id, file("Samp_id.txt"))
