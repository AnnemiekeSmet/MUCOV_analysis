setwd("/Path/to/dataset.txt/")
getwd()
install.packages("ggplot2")
install.packages("tidyverse")
library(ggplot2)
library(tidyverse)
df <- read.table("severity_MUC_log.txt", header=T, sep="\t", dec=",")
pMUC <- ggplot(df, aes(x = dyspnea, y = MUC2)) 
pMUC <- pMUC + geom_point(size = 3, alpha = 1, na.rm = T, fill = "#56B4E9", shape = 21, colour = "black") + geom_smooth(method = lm, size = 1) + theme(axis.text.x = element_text(size=12, face="bold", color="black")) + theme(axis.text.y = element_text(size=12, face="bold", color="black")) +
  theme(legend.text = element_text(size=10, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold")) + 
  theme(axis.title.x = element_text(size=14, face="bold")) + labs(x="legend title x-axis", y="legend title y-axis") +  
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black")) + theme(panel.background = element_blank()) 
print(pMUC)