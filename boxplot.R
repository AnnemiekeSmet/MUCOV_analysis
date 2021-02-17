setwd("/Path/to/dataset.txt/")
getwd()
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
df <- read.table("hospitalisation.txt", header=T, sep="\t")
box_plot <- ggplot(df, aes(x = hospitalisation, y = days)) + geom_boxplot(size=0.8, colour="#56B4E9") + coord_flip() + theme(axis.text.x = element_text(size=13, face="bold", colour = "black")) + 
  theme(axis.text.y = element_text(size=13, face="bold", colour = "black")) +
  theme(axis.title.x = element_text(size=13, face="bold")) + 
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90)) + theme(axis.line = element_line(colour = "black")) +
  theme(axis.title.y = element_blank())  
print(box_plot)
 
theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(panel.background = element_blank())
