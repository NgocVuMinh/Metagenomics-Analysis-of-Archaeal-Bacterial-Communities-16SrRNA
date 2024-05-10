#install.packages("ggsignif")
# BiocManager::install(c("microbiome", "ComplexHeatmap"), update = FALSE)
#install.packages(
#  "microViz",
#  repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos"))
#)

library(ggforce)
library(ggsignif)
library(ggpubr)
library(microViz)


ntaxa(ps) # = length(unique(melt$OTU))


# Abundance per phylum per sample
arch.phyl.abun <- melt%>%
  filter(Kingdom == "Archaea")%>%
  group_by(Sample, Phylum) %>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate('%' = round(100 * Abundance / sum(Abundance), 3))
bac.phyl.abun <- melt%>%
  filter(Kingdom == "Bacteria")%>%
  group_by(Sample, Phylum) %>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate('%' = round(100 * Abundance / sum(Abundance), 3))
genus.abun <- melt%>%
  group_by(Sample, Genus) %>%
  filter(Kingdom == "Archaea")%>%
  summarise(Abundance = sum(Abundance)) %>%
  mutate('%' = round(100 * Abundance / sum(Abundance), 3))


# _____________ ALPHA DIVERSITY _____________ 

ps.bac <- tax_select(ps, c("Bacteria"),
                     ranks_searched = "all",
                     strict_matches = FALSE,
                     n_typos = 1, deselect = FALSE
                     )

ps.arch <- tax_select(ps, c("Archaea"),
                     ranks_searched = "all",
                     strict_matches = FALSE,
                     n_typos = 1, deselect = FALSE
                     )


# alpha: untrimmed, non-normalized data
plot_richness(ps.bac, x = "samples", 
              measures = c("Chao1", "Observed", "Shannon"),
              color = "Location") + 
  geom_point(size = 5)

plot_richness(ps.arch, x = "samples", 
              measures = c("Chao1", "Observed", "Shannon"),
              color = "Location") + 
  geom_point(size = 5)

alpha_values.bac <- estimate_richness(ps.bac, measures = c("Observed", "Shannon", "Chao1"))
alpha_values.arch <- estimate_richness(ps.arch, measures = c("Observed", "Shannon", "Chao1"))

alpha_values.bac$e <- substr(rownames(alpha_values.bac), 1, 2)
alpha_values.bac$Location[alpha_values.bac$e == "AF" | alpha_values.bac$e == "AM" | alpha_values.bac$e == "BR" | alpha_values.bac$e == "PO" | alpha_values.bac$e == "FU"] <- "Corals"
alpha_values.bac$Location[alpha_values.bac$e == "SD" ] <- "Sediment"
alpha_values.bac$Location[alpha_values.bac$e == "WC" ] <- "Water column"
alpha_values.bac <- alpha_values.bac[, -5]

alpha_values.arch$e <- substr(rownames(alpha_values.arch), 1, 2)
alpha_values.arch$Location[alpha_values.arch$e == "AF" | alpha_values.arch$e == "AM" | alpha_values.arch$e == "BR" | alpha_values.arch$e == "PO" | alpha_values.arch$e == "FU"] <- "Corals"
alpha_values.arch$Location[alpha_values.arch$e == "SD" ] <- "Sediment"
alpha_values.arch$Location[alpha_values.arch$e == "WC" ] <- "Water column"
alpha_values.arch <- alpha_values.arch[, -5]

alpha_values2 <- plot_richness(ps, x = "samples", 
              measures = c("Chao1", "Observed", "Shannon"),
              color = "Location")$data
alpha_values2.bac <- plot_richness(ps.bac, x = "samples", 
                               measures = c("Chao1", "Observed", "Shannon"),
                               color = "Location")$data
alpha_values2.arch <- plot_richness(ps.arch, x = "samples", 
                               measures = c("Chao1", "Observed", "Shannon"),
                               color = "Location")$data

# _____________ Statistical testing _____________ 

# shapiro-wilk test to see whether the data is normally distributed
# alpha_diversity: 1. boxplot; 2. kruskal test (non-param), 3. t-test/ANOVA
# beta_diversity: 1. boxplot; 2. anosim
comp = list( c("Sediment", "Water column"),
            c("Corals", "Sediment"),
            c("Corals", "Water column"))
ggplot(alpha_values.bac, mapping = aes(x = Location, y = Observed, fill= Location)) + 
  geom_boxplot(width = 0.3) + theme_bw() + theme(legend.position="none") +
  stat_compare_means(comparisons = comp, label = "p.signif")
                     #method = "t.test",
                     

# all 3 metrics
grid.arrange(ggplot(alpha_values.bac, mapping = aes(x = Location, y = Observed, fill= Location)) + 
               geom_boxplot(width = 0.3) + theme_bw() + theme(legend.position="none")  +
               stat_compare_means(comparisons = comp, label = "p.signif"),
             ggplot(alpha_values.bac, mapping = aes(x = Location, y = Shannon, fill= Location)) + 
               geom_boxplot(width = 0.3) + theme_bw() + theme(legend.position="none")  +
               stat_compare_means(comparisons = comp, label = "p.signif"),
             ggplot(alpha_values.bac, mapping = aes(x = Location, y = Chao1, fill= Location)) + 
               geom_boxplot(width = 0.3) + theme_bw() + theme(legend.position="none")  +
               stat_compare_means(comparisons = comp, label = "p.signif"),
             ncol = 3)

# omit Observed (similar to Chao1)
alpha_values2.bac$value <- as.numeric(alpha_values2.bac$value)
alpha_values2.bac$Location <- as.factor(alpha_values2.bac$Location)
alpha_values2.bac%>%
  select(Location, samples, variable, value)%>%
  #filter(variable != 'Observed')%>%
  ggplot(mapping = aes(x = Location, y = value, color = Location)) +
  geom_boxplot(width = 0.5) +
  geom_point(mapping = aes(x = Location, y = value, color = Location), size = 2.5) +
  facet_wrap(~variable, scales = "free",
             labeller = labeller(variable = c(Observed = "Observed (p=0.0066)", 
                                           Chao1 = "Chao1 (p=0.0065)",
                                           Shannon = "Shannon (p=0.00074)"))) +
  labs(title = "Bacteria") +
  #xlab("Bacteria") +
  theme_bw() +
  theme(
    #axis.text.x=element_blank(),
    #axis.text.x = element_text(angle = -30),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  ylab("Alpha Diversity Measure") +
  stat_compare_means(comparisons = comp, 
                     label = "p.signif")   #, method="t.test"
  #stat_compare_means(label = "p.format", method="anova",label.x.npc = c("right"))


alpha_values2.arch%>%
  select(Location, samples, variable, value)%>%
  #filter(variable != 'Observed')%>%
  ggplot(mapping = aes(x = Location, y = value, color = Location)) +
  geom_boxplot(width = 0.5) +
  geom_point(mapping = aes(x = Location, y = value, color = Location), size = 2.5) +
  facet_wrap(~variable, scales = "free",
             labeller = labeller(variable = c(Observed = "Observed (p=0.00019)", 
                                              Chao1 = "Chao1 (p=0.00019)",
                                              Shannon = "Shannon (p=3.6e-05)"))) +
  labs(title = "Archaea") +
  theme_bw() +
  theme(
    #axis.text.x=element_blank(),
    #axis.text.x = element_text(angle = -30),
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank()
  ) +
  ylab("Alpha Diversity Measure") +
  stat_compare_means(comparisons = comp, 
                     label = "p.signif")


shapiro.test(filter(alpha_values2.bac, variable == "Observed")$value) # p-value = 0.06378, normally distributed
shapiro.test(filter(alpha_values2.bac, variable == "Chao1")$value) # p-value = 0.06054, normally distributed
shapiro.test(filter(alpha_values2.bac, variable == "Shannon")$value) # p-value = 0.08857, normally distributed
# -> all normally distributed

# Kruskal Wallis test for not normally distributed
kruskal.test(value~Location, filter(alpha_values2.bac, variable == "Observed"))
# p = ? -> no/yes signif difference
kruskal.test(value~Location, filter(alpha_values2.bac, variable == "Chao1"))
# p = ? -> no/yes signif difference

# ANOVA test for normally distributed
summary(aov(value~Location, data = filter(alpha_values2.bac, variable == "Shannon" & Location != "Sediment")))
summary(aov(value~Location, data = filter(alpha_values2.arch, variable == "Shannon" & Location != "Water column")))
# p = ? => -> no/yes signif difference


anosim(subset(otu.matrix, select = -Location), otu.matrix$Location, distance = "bray", permutations = 9999)

BC.dis <- phyloseq::distance(ps3, method="bray")
PS.ord = ordinate(ps3, method="NMDS", distance=BC.dis)
BC.dis
PS.ord

anosim(BC.dis, sample_data(ps3)$Location, permutations = 1000)
anosim(BC.dis, sample_data(ps3.1)$Location, permutations = 1000)
#p = ? => ?

