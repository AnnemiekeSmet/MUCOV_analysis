setwd("/Path/to/dataset/")
getwd()
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("RColorBrewer")
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
df <- read.table("histogram_symptoms.txt", header=T, sep="\t")
df_new <- df    # Replicate data                          
df_new$disease <- factor(df_new$disease, levels = c("severe COVID-19", "mild COVID-19", "mild non-COVID-19")) # Reordering group factor levels
phisto <- ggplot(df_new, aes(x=symptoms, fill=disease))
phisto <- phisto + geom_bar(aes(fill=disease), width = 0.5) + 
  geom_text(stat = 'count',aes(label =..count.., vjust = -0.3)) + 
  scale_y_continuous(limits = c(0, 25)) +
  theme(axis.text.x = element_text(angle=90, vjust=0.4, hjust=1, size=12, face="bold", colour = "black")) + 
  theme(axis.text.y = element_text(size=10, face="bold", colour = "black")) +
  theme(legend.text = element_text(size=10, face="bold")) +
  theme(axis.title.y = element_text(size=14, face="bold")) + 
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00", "#1B9E77")) +
  facet_grid(disease ~ .)
print(phisto)
