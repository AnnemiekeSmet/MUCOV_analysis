setwd("/Path/to/dataset/")
getwd()
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("RColorBrewer")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
df <- read.table("histogram_co_morbidities.txt", header=T, sep="\t")
phisto <- ggplot(df, aes(x=morbidity, fill=disease)) +
  geom_bar(aes(fill=disease), width = 0.5) +
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.3)) + 
  scale_y_continuous(limits = c(0, 45)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.4, hjust=1, size=13, face="bold", colour = "black")) + 
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black")) +
  theme(legend.text = element_text(size=10, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold")) + 
  theme(axis.title.x = element_blank()) + 
  scale_fill_manual(values=c("#56B4E9")) +
  theme(legend.text = element_blank()) + theme(legend.position="none")
print(phisto)


theme(panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")) + theme(panel.background = element_blank())
