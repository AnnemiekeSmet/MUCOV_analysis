setwd("/Path/to/dataset.txt/")
getwd()
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("RColorBrewer")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
df <- read.table("violin_plot.txt", header=T, sep="\t")
pviolin <- ggplot(df, aes(x=disease, y=age, fill=gender)) +
  geom_violin() + 
  geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(1), binwidth = 1.5) +
  scale_x_discrete(limits=c("severe COVID-19", "mild COVID-19", "mild non-COVID-19", "healthy control")) +
  scale_y_continuous(breaks = c(10,20,30,40,50,60,70,80,90)) +
  theme(axis.text.x = element_text(size=11, face="bold", colour = "black")) + 
  theme(axis.text.y = element_text(size=12, face="bold", colour = "black")) +
  theme(legend.text = element_text(size=10, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold")) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position="bottom") 
print(pviolin)