library(vegan)

# beta
sample_sums(ps)
min(sample_sums(ps))
set.seed(9999)

# RAREFYING IS NOT RECOMMENDED BUT HERE IS THE CODE ANYWAY
ps_rarefy.bac <- rarefy_even_depth(ps.bac, rngseed = T) 
ps_rarefy_ss.bac <- rarefy_even_depth(ps.bac, rngseed = T, sample.size = min(sample_sums(ps.bac))) 
ps_rarefy.arc <- rarefy_even_depth(ps.arch, rngseed = T) 
ps_rarefy_ss.arc <- rarefy_even_depth(ps.arch, rngseed = T, sample.size = min(sample_sums(ps.arch))) 


#### DIFFERENT SCALING STRATEGIES: CHOOSE WHATEVER BEST FOR UR DATA
ps.log <- transform_sample_counts(ps.bac, function(x) log10(max(x+1) - x))
ps.log.bac <- transform_sample_counts(ps.bac, function(x) log2(x+1))
ps.log.arch <- transform_sample_counts(ps.arch, function(x) log2(x+1))

ord.nmds.log.bac <- ordinate(ps.log.bac, method = "NMDS", distance = "bray") 
ord.nmds.log.arch <- ordinate(ps.log.arch, method = "NMDS", distance = "bray") 
ord.pcoa.log.bac <- ordinate(ps.log.bac, method = "PCoA", distance = "bray")
ord.pcoa.log.arch <- ordinate(ps.log.arch, method = "PCoA", distance = "bray")

# Plot distance using NMDS or PCOA, here PCOA is prefered:
# ordinate() would return stress values, the best case: 0-0.05; if stress>0.2 => use pcoa
plot_ordination(ps.log.bac, ord.nmds.log.bac, color = "Location") + 
  geom_point(size = 3) +
  geom_mark_ellipse()
plot_ordination(ps.log.arch, ord.nmds.log.arch, color = "Location") + 
  geom_point(size = 3) +
  geom_mark_ellipse()

plot_ordination(ps.log.bac, ord.pcoa.log.bac, color = "Location") + 
  geom_point(size = 4) + geom_mark_ellipse() +
  theme(
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14),
    panel.grid.major = element_blank()#, panel.grid.minor = element_blank()
  ) +
  labs(title = "Bacteria")
plot_ordination(ps.log.arch, ord.pcoa.log.arch, color = "Location") + 
  geom_point(size = 4) + geom_mark_ellipse() +
  theme(
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14),
    panel.grid.major = element_blank()#, panel.grid.minor = element_blank()
  ) +
  labs(title = "Archaea")


grid.arrange(
  plot_ordination(ps.log.arch, ord.pcoa.log.arch, color = "Location") + 
  geom_point(size = 3),
  plot_ordination(ps_rarefy.arc, ord.pcoa.log.arch, color="Location") + 
  geom_point(size = 3))

# ______________ For RAREFYING, again, NOT RECOMMENDED _______________
ord.nmds.arch <- ordinate(ps_rarefy.arc, method = "NMDS", distance = "bray")
ord.nmds_ss.arch <- ordinate(ps_rarefy_ss.arc, method = "NMDS", distance = "bray")
ord.nmds.bac <- ordinate(ps_rarefy, method = "NMDS", distance = "bray")
ord.nmds_ss.bac <- ordinate(ps_rarefy_ss.bac, method = "NMDS", distance = "bray")

ord.pcoa.bac <- ordinate(ps_rarefy.bac, method = "PCoA", distance = "bray")
ord.pcoa_ss.bac <- ordinate(ps_rarefy_ss.bac, method = "PCoA", distance = "bray")
ord.pcoa.arch <- ordinate(ps_rarefy.arc, method = "PCoA", distance = "bray")
ord.pcoa_ss.arch <- ordinate(ps_rarefy_ss.arc, method = "PCoA", distance = "bray")


### pcoa: normal, nmds: insufficient data
plot_ordination(ps_rarefy.bac, ord.nmds.bac, color="Location") + 
  geom_point(size = 4) + geom_mark_ellipse() +
  theme(
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14),
    panel.grid.major = element_blank()#, panel.grid.minor = element_blank()
  ) +
  labs(title = "Bacteria")
plot_ordination(ps_rarefy.arc, ord.nmds.arch, color="Location") + 
  geom_point(size = 4) + geom_mark_ellipse() +
  theme(
    text = element_text(size = 14), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14), 
    axis.title.x = element_text(vjust=-1.7),
    axis.title.y = element_text(vjust=1.7),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),
    strip.text = element_text(size=14),
    panel.grid.major = element_blank()#, panel.grid.minor = element_blank()
  ) +
  labs(title = "Archaea")


# _______________ Statistical testing ________________


BC.dis.bac <- phyloseq::distance(ps.log.bac, method="bray")
BC.dis.arch <- phyloseq::distance(ps.log.arch, method="bray")
PS.ord.bac = ordinate(ps.log.bac, method="NMDS", distance=BC.dis.bac)
PS.ord.arch = ordinate(ps.log.arch, method="NMDS", distance=BC.dis.arch)


anosim(BC.dis.bac, sample_data(ps.log.bac)$Location, permutations = 1000) 
#ANOSIM statistic R: 0.578 
#Significance: 0.000999 

anosim(BC.dis.arch, sample_data(ps.log.arch)$Location, permutations = 1000) 
#ANOSIM statistic R: 0.4801 
#Significance: 0.001998 

