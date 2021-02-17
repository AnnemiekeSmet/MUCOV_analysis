setwd("/Path/to/dataset/")
getwd()

install.packages("gplots")
install.packages("RColorBrewer")
library(gplots)
library(RColorBrewer)
data <- read.table("heatmap_log.txt", header=T, sep="\t")
rnames <- data[,1]                            
mat_data <- data.matrix(data[,2:ncol(data)])  
rownames(mat_data) <- rnames                  
my_palette <- colorRampPalette(brewer.pal(8, "GnBu"))(25)
png("heatmaps_in_r.png", width = 5*300, height = 5*300, res = 300, pointsize = 8) 
heatmap.2(mat_data,
          main = "mucin mRNA expression", # heat map title
          notecol="black",      # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace="none",         # turns off trace lines inside the heat map
          margins =c(10,8),     # widens margins around plot
          col=my_palette,       # use on color palette defined earlier
          dendrogram="none",
          cexRow = 1, 
          cexCol = 1.2, 
          Colv="NA")            # turn off column clustering

dev.off()  
