###################
# ADMIXTURE results
###################

# Load in necessary libraries

library(reshape2)
library(ggplot2)

# Read in data

data <- read.delim("./admixture_results.txt", header=TRUE)

# Melt data

melt_data <- melt(data, id=c("ordered_vines", "morphospecies"))

# Plot data

p <- ggplot(melt_data, aes(x=as.factor(ordered_vines), y=value, fill=variable))

p + geom_bar(stat="identity", position="fill") +

facet_wrap(. ~ morphospecies, drop=TRUE, scales="free") +

theme(axis.text.x = element_text(face = "plain", color = "black", size = 3, angle = 90)) +

scale_fill_manual(values=c("#b3de69","#bc80bd","#d9d9d9","#fdb462","#80b1d3","#bebada","#8dd3c7","#fccde5","#ffffb3","#fb8072"))+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank())

###################
# ADMIXTURE PCA
###################

# Load in necessary libraries

library(ggrepel)
library(ggplot2)

# Read in data

data <- read.delim("./admixture_pca.txt", header=TRUE)


# Subset data for all known species, the mystery Vitis spp., and questionable vines

species <- subset(data, morphospecies!="Vitis_spp")

vitis_spp <- subset(data, morphospecies=="Vitis_spp")

question <- subset(data, questionable!="NA")

# Plot PC1 and PC2
# Remove hastag to visualize questionable vines

p <- ggplot(data=species, aes(x=PC1, y=PC2, color=morphospecies))

p + geom_point(size=8, alpha=0.5) +
geom_point(data=vitis_spp, color="black", size=4, alpha=0.75) +
geom_text_repel(data=vitis_spp, aes(x=PC1, y=PC2, label=ordered_vines, textsize=240), size=7.5) +
# geom_text_repel(data=question, aes(x=PC1, y=PC2, label=questionable)) +
scale_colour_manual(values=c("#e41a1c","#ff7f00","#ffff33","#4daf4a","#377eb8","#984ea3","#f781bf","#999999","#a65628","black")) + theme_bw()

# Plot PC3 and PC4

p <- ggplot(data=species, aes(x=PC3, y=PC4, color=morphospecies))

p + geom_point(size=8, alpha=0.5) +
geom_point(data=vitis_spp, color="black") +
geom_text_repel(data=vitis_spp, aes(x=PC3, y=PC4, label=ordered_vines, size = 12), size=7.5) +
# geom_text_repel(data=question, aes(x=PC3, y=PC4, label=questionable)) +
scale_colour_manual(values=c("#e41a1c","#ff7f00","#ffff33","#4daf4a","#377eb8","#984ea3","#f781bf","#999999","#a65628","black")) + theme_bw()



