################################################################
# Morphospace comparison of V. riparia, V. labrusca, and hybrids
################################################################

# Load in necessary libraries

library(ggplot2)
library(ggrepel)

# Read in data

data <- read.csv("Modeled_reformatted.csv", header=TRUE)

# Subset data by species

riparia <- droplevels(subset(data, data$species == "Vitis_riparia"))
labrusca <- droplevels(subset(data, data$species == "Vitis_labrusca"))
novae_angliae <- droplevels(subset(data, data$species == "Vitis_x_novae_angliae"))

# Create a dataframe of just V. riparia, V. labrusca, and V. novae-angliae

riparia_labrusca_novae <- rbind(riparia, labrusca, novae_angliae)

# Average leaf shapes across years and accessions for each vine

mean_rip_lab_nov <- aggregate(riparia_labrusca_novae[c(5:424)], riparia_labrusca_novae[1], FUN=mean)

just_species <- unique(riparia_labrusca_novae[c(1,4)])

merge_rip_lab_nov <- merge(mean_rip_lab_nov, just_species)

# Perform a PCA on averaged vine information

pca <- prcomp(merge_rip_lab_nov[2:421])

summary(pca)

# Combine PCA scores and vine information

pca_scores <- cbind(merge_rip_lab_nov[c(1,422)],pca$x)

# Remove V. novae-angliae (in order to display with other hybrids)

no_novae_pca_scores <- subset(pca_scores, pca_scores$species != "Vitis_x_novae_angliae")

# Isolate PCA scores for V. riparia hybrids and V. novae-angliae individually

v483175 <- droplevels(subset(pca_scores, pca_scores$vine == "483175")) # riparia-labrusca hybrid
v483181 <- droplevels(subset(pca_scores, pca_scores$vine == "483181")) # riparia-labrusca hybrid
v483177 <- droplevels(subset(pca_scores, pca_scores$vine == "483177")) # riparia-labrusca hybrid
v588257 <- droplevels(subset(pca_scores, pca_scores$vine == "588257")) # Vitis_x_novae_angliae

hybrid_pca_scores <- rbind(v483175, v483181, v483177, v588257)

# Plot PC values and add text to highlight hybrid vines

p <- ggplot(no_novae_pca_scores, aes(PC1, PC2, color=species))
p + geom_point(size=5, alpha=0.5) + stat_ellipse(size=2, alpha=0.5) + geom_point(data=hybrid_pca_scores, aes(PC1, PC2), color="black", size=3) + geom_text_repel(data=hybrid_pca_scores, aes(PC1, PC2, label=vine), color="black", size=6) + theme_bw() + scale_color_manual(values=c("orange", "dodgerblue"))

ggsave("morphospace.jpg")

#######################
# Comparing leaf shapes
#######################

# Load in necessary libraries

library(ggplot2)

# Read in data

data <- read.csv("Procrustes_all.csv", header=TRUE)

# Subset V. ripariua, V. labrusca, and hybrids

riparia <- droplevels(subset(data, species=="Vitis_riparia"))
labrusca <- droplevels(subset(data, species=="Vitis_labrusca"))
v588257 <- droplevels(subset(data, vine=="588257")) # Vitis novae-angliae
v483175 <- droplevels(subset(data, vine=="483175"))
v483181 <- droplevels(subset(data, vine=="483181"))
v483177 <- droplevels(subset(data, vine=="483177"))

# For hybrid vines, rename the factor "species" by the vine ID

v588257$species <- "v588257"
v483175$species <- "v483175"
v483181$species <- "v483181"
v483177$species <- "v483177"

# Create new dataset of parents and hybrids

parents_hybrids <- rbind(riparia, labrusca, v588257, v483175, v483181, v483177)

# Take means of all leaves for each species/vine

means <- aggregate(parents_hybrids[c(51:92)], parents_hybrids[4], FUN=mean)

# Rename the dataframe "means" as "shapes" so that specific species/vines can be selected

shapes <- means[]

# Plot data

p <- ggplot(shapes, aes(x=x1, y=y1, xend=x6, yend=y6, color=species))
p + geom_segment() +
geom_segment(aes(x=x6, y=y6, xend=x14, yend=y14, color=species)) +
geom_segment(aes(x=x14, y=y14, xend=x5, yend=y5, color=species)) +
geom_segment(aes(x=x5, y=y5, xend=x15, yend=y15, color=species)) +
geom_segment(aes(x=x15, y=y15, xend=x7, yend=y7, color=species)) +
geom_segment(aes(x=x7, y=y7, xend=x2, yend=y2, color=species)) +
geom_segment(aes(x=x2, y=y2, xend=x9, yend=y9, color=species)) +
geom_segment(aes(x=x9, y=y9, xend=x17, yend=y17, color=species)) +
geom_segment(aes(x=x17, y=y17, xend=x8, yend=y8, color=species)) +
geom_segment(aes(x=x8, y=y8, xend=x18, yend=y18, color=species)) +
geom_segment(aes(x=x18, y=y18, xend=x10, yend=y10, color=species)) +
geom_segment(aes(x=x10, y=y10, xend=x3, yend=y3, color=species)) +
geom_segment(aes(x=x3, y=y3, xend=x12, yend=y12, color=species)) +
geom_segment(aes(x=x12, y=y12, xend=x20, yend=y20, color=species)) +
geom_segment(aes(x=x20, y=y20, xend=x11, yend=y11, color=species)) +
geom_segment(aes(x=x11, y=y11, xend=x21, yend=y21, color=species)) +
geom_segment(aes(x=x21, y=y21, xend=x13, yend=y13, color=species)) +
geom_segment(aes(x=x13, y=y13, xend=x4, yend=y4, color=species)) +

geom_segment(aes(x=x14, y=y14, xend=x15, yend=y15, color=species)) +
geom_segment(aes(x=x15, y=y15, xend=x16, yend=y16, color=species)) +
geom_segment(aes(x=x16, y=y16, xend=x17, yend=y17, color=species)) +
geom_segment(aes(x=x17, y=y17, xend=x18, yend=y18, color=species)) +
geom_segment(aes(x=x18, y=y18, xend=x19, yend=y19, color=species)) +
geom_segment(aes(x=x19, y=y19, xend=x20, yend=y20, color=species)) +
geom_segment(aes(x=x20, y=y20, xend=x21, yend=y21, color=species)) +

theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.title.x=element_blank(), axis.text.x=element_blank(),
axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(),
axis.ticks.y=element_blank()) + scale_colour_manual(values=c("orange", "dodgerblue", "gray", "gray", "gray", "gray")) + coord_fixed()

ggsave("vALL.jpg")






