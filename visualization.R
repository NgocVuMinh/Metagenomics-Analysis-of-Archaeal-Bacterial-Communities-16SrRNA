#BiocManager::install("Heatplus")
#install.packages("gplots")

library(dplyr)
library(tidyr)
library(Heatplus)
library(vegan)
library(gplots)
theme_set(theme_bw())

# Get the composition in the form of matrix at different levels for all samples:

king_summarized_data <- king100qt %>%
  group_by(Sample, Kingdom) %>%
  summarise(Sum_Abundance = sum(Abundance))
king_pivot_table <- king_summarized_data %>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)
# write_csv(king_pivot_table, "kingdom.csv")

bacphy.pivot <- bacphy100qt %>%
  group_by(Sample, Phylum) %>%
  summarise(Sum_Abundance = sum(Abundance))%>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)
archphy.sum <- archphy100qt%>%
  group_by(Sample, Phylum)%>%
  summarise(Sum_Abundance = sum(Abundance))%>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)

archclass.pivot <- archclass100qt %>%
  group_by(Sample, Class) %>%
  summarise(Sum_Abundance = sum(Abundance))%>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)
archorder.pivot <- archorder100qt %>%
  group_by(Sample, Order) %>%
  summarise(Sum_Abundance = sum(Abundance))%>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)


##############

archphy.sum <- ps%>%
  tax_glom(taxrank = "Phylum")%>% # agglomerate data of the same phylum
  #transform_sample_counts(function(x) {x/sum(x)})%>% 
  psmelt()%>% # combine format(?), melt and merge all into a dataframe
  filter(Abundance > 0.002)%>%
  arrange(Phylum)%>%
  filter(Kingdom == "Archaea")

View(archphy.sum%>%
       group_by(Sample, Phylum)%>%
       summarise(Sum_Abundance = sum(Abundance))%>%
       pivot_wider(names_from = Sample, values_from = Sum_Abundance))


# _______________ PLOTTING ________________

# ______________ (1) COMPOSIONAL STACKED-BAR PLOTS _______________
# You could choose either to use (1) stacked-bar plots or (2) heatmaps 

theme_set(theme_bw())

# Kingdom Composition
ggplot(data = king100qt, aes(x = Sample, y = Abundance, fill = Kingdom)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#CC0000", "#99BADD")) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Kingdom composition") +
  facet_wrap(~Location, nrow = 1, scales = "free_x") +
  theme(axis.text.x = element_text(angle = -45)) +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))

# Phylum Composition
ggplot(data = bacphy100qt, aes(x = Sample, y = Abundance, fill = Phylum)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[3:50]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Phylum Composition of Bacteria") +
  facet_wrap(~Location, nrow = 1, scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))



ggplot(data = archphy100qt, aes(x = Sample, y = Abundance, fill = Phylum)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[5:30]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Phylum Composition of Archaea") +
  facet_wrap(~Location, nrow = 1, scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))


# Class Composition
ggplot(data = archclass100qt, aes(x = Sample, y = Abundance, fill = Class)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[10:20]) +
  facet_wrap(~Location, nrow = 1, scales = "free_x") +
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Class Composition of Archaea") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))
 
ggplot(data = bacclass100qt, aes(x = Sample, y = Abundance, fill = Class)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[3:100]) +
  facet_wrap(~Location, nrow = 1, scales = "free_x") +
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Class Composition of Bacteria") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=2))

# Order Composition
ggplot(data = bacorder100qt, aes(x = Sample, y = Abundance, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[4:130]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Order Composition of Bacteria") +
  theme(axis.text.x = element_text(angle = -45)) +
  facet_wrap(~Location, nrow = 1, scale ="free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 8),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))

ggplot(data = archorder100qt, aes(x = Sample, y = Abundance, fill = Order)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[12:100]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Order Composition of Archaea") +
  theme(axis.text.x = element_text(angle = -45)) +
  facet_wrap(~Location, nrow = 1, scale ="free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))

# Family Composition
ggplot(data = bacfam100qt, aes(x = Sample, y = Abundance, fill = Family)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[2:130]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Family Composition of Bacteria") +
 theme(axis.text.x = element_text(angle = -45)) +
  facet_wrap(~Location, nrow = 1, scale ="free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 8),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=4))

ggplot(data = archfam100qt, aes(x = Sample, y = Abundance, fill = Family)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[7:30]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Family Composition of Archaea") +
  theme(axis.text.x = element_text(angle = -45)) +
  facet_wrap(~Location, nrow = 1, scale ="free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))

# Genus Composition
ggplot(data = bacgenus100qt, aes(x = Sample, y = Abundance, fill = Genus,)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Genus Composition of Bacteria") +
  theme(axis.text.x = element_text(angle = -45)) +
  facet_wrap(~Location, nrow = 1, scale ="free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=3))

ggplot(data = archgenus100qt, aes(x = Sample, y = Abundance, fill = Genus,)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors152[15:20]) + 
  ylab("Relative Abundance") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(title = "Genus Composition of Archaea") +
  theme(axis.text.x = element_text(angle = -45)) +
  facet_wrap(~Location, nrow = 1, scale ="free_x") +
  theme(
    axis.text.x = element_text(angle = -45),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14)
  ) +
  guides(fill=guide_legend(ncol=1))


# ______________ (2) COMPOSIONAL HEATMAPS AT CLASS AND GENUS LEVEL _______________
# You could choose either to use (1) stacked-bar plots or (2) heatmaps 

class_summarized_data <- bacclass100qt %>%
  group_by(Sample, Class) %>%
  summarise(Sum_Abundance = sum(Abundance))
class_pivot_table <- class_summarized_data %>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)
class_pivot_table[is.na(class_pivot_table)] <- 0
class_pivot_table <- t(class_pivot_table)
#class_pivot_table <- as.data.frame(class_pivot_table)
colnames(class_pivot_table) <- class_pivot_table[1,]
class_pivot_table <- class_pivot_table[-1,]
class_pivot_table <- apply(class_pivot_table, 1:2, as.numeric)
heatmap(as.matrix(class_pivot_table), 
        Rowv = NA, Colv = NA, 
        col = scaleyellowred)

# Only including the most prevalent classes in the heatmap
class.max <- apply(class_pivot_table, 2, max)
n1 <- names(class.max[class.max < 0.015])
class.pivot.clean <- class_pivot_table[,-which(colnames(class_pivot_table) %in% n1)]
class.pivot.clean <- t(class.pivot.clean)
heatmap(as.matrix(t(class.pivot.clean)), 
        Rowv = NA, Colv = NA, col = scaleyellowred,
        margins = c(10, 10))
heatmap.2(as.matrix(class.pivot.clean), 
          Colv = NA, Rowv = NA, 
          col = scaleyellowred,
          trace = "none", density.info = "none",
          margins = c(10, 10), ColSideColors = var1
          #xlab = "genera", ylab = "Samples", main = "Heatmap example" 
)

# Annotation for groups of samples:
var1 <- colnames(class.pivot.clean)
var1 <- replace(var1, grepl("AF|AM|BR", var1), '#D2D2D2')
var1 <- replace(var1, grepl("SD", var1), "#A0A0A0")
var1 <- replace(var1, grepl("WC", var1), "#3C3C3C")
cbind(colnames(class.pivot.clean), var1)


order_summarized_data <- archorder100qt %>%
  group_by(Sample, Order) %>%
  summarise(Sum_Abundance = sum(Abundance))
order_pivot_table <- order_summarized_data %>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)

# Same heatmap for genus:
genus_summarized_data <- bacgenus100qt %>%
  group_by(Sample, Genus) %>%
  summarise(Sum_Abundance = sum(Abundance))
genus_pivot_table <- genus_summarized_data %>%
  pivot_wider(names_from = Sample, values_from = Sum_Abundance)

genus_pivot_table[is.na(genus_pivot_table)] <- 0
genus_pivot_table <- t(genus_pivot_table)
colnames(genus_pivot_table) <- genus_pivot_table[1,]
genus_pivot_table <- genus_pivot_table[-1,]
genus_pivot_table <- as.data.frame(genus_pivot_table)
genus_pivot_table <- apply(genus_pivot_table, 1:2, as.numeric)
#row.names(genus_pivot_table) <- genus_pivot_table$Genus

scaleyellowred <- colorRampPalette(c("lightyellow", "red"), space = "rgb")(100)
heatmap(as.matrix(genus_pivot_table), Rowv = NA, Colv = NA, col = scaleyellowred)
genus.max <- apply(genus_pivot_table, 2, max)
n1 <- names(genus.max[genus.max < 0.05])
genus.pivot.clean <- genus_pivot_table[, -which(colnames(genus_pivot_table) %in% n1)]
#genus.pivot.clean <- apply(genus.pivot.clean, numeric)
heatmap(as.matrix(t(genus.pivot.clean)), 
        Rowv = NA, Colv = NA, col = scaleyellowred)

cbind(row.names(data.prop), var1)

heatmap.2(as.matrix(t(genus.pivot.clean)), 
          Colv = NA, Rowv = NA, 
          col = scaleyellowred,
          trace = "none", density.info = "none" 
          #xlab = "genera", ylab = "Samples", main = "Heatmap example" 
)

