
# ____________ LOADING PACKAGES ____________

library(tidyverse)
library(phyloseq)
library(dada2)
library(Rcpp)
library(ggplot2)
library(dplyr)
library(treemap)
library(viridis)
library(RColorBrewer)
library(ggforce)
library(gridExtra)
library(grid)


# ____________ This block of code is to load and save variables ____________

# setwd("/Volumes/T7/archaea/")
# load("/Volumes/T7/archaea/envs/run4.RData")
# save.image("/Volumes/T7/archaea/envs/run4.RData")

# ____________ Writing data ____________
path = "/Volumes/T7/archaea/data3"  #  create a path for writing and downloading data
list.files(path)  # list all file names in folder database

# forward
fnFs <- sort(list.files(path = path, pattern="_R1.fastq", full.names = T))
# reverse
fnRs <- sort(list.files(path = path, pattern="_R2.fastq", full.names= T))
summary(fnFs)
summary(fnRs)
fnFs
fnRs

basename(fnFs) # fnFs returns sra/filename but basename() returns filename only
sample.names <- sapply(strsplit(basename(fnFs), "_"), `[`, 1)
sample.names  
# class(sample.names)

# assess sequencing quality to determine trimming and filtering thresholds
plotQualityProfile(fnFs[1: 9])
plotQualityProfile(fnRs[1: 9]) 

# create a place to store filtered files ("filtered" directory)
filtFs <- file.path(path, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
filtRs <- file.path(path, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))
filtFs
filtRs


# filter and trim reads
out <- filterAndTrim(fnFs, filtFs, fnRs, filtRs, #truncLen = c(200,200),
                     maxN = 0, maxEE = c(2,2), truncQ = 2, trimLeft = c(15,15),
                     trimRight = c(10,30),
                     rm.phix = TRUE, compress = TRUE, multithread = T) 
View(out)


# ______________ Learn the Error Rates _________________

errF <- learnErrors(filtFs, multithread = T)  # calculating the sequence error rates of reads
errR <- learnErrors(filtRs, multithread = T)
plotErrors(errF, nominalQ=TRUE)
plotErrors(errR, nominalQ=TRUE)

# number of unique sequences?
derepFs <- derepFastq(filtFs, verbose=TRUE) 
derepRs <- derepFastq(filtRs, verbose=TRUE)

# Name the derep-class objects by the sample names
names(derepFs) <- sample.names 
names(derepRs) <- sample.names

derepFs[[1]]

# number of reads in ? unique sequence
dadaFs <- dada(derepFs, err=errF, multithread=T) 
dadaRs <- dada(derepRs, err=errR, multithread=T)
dadaFs

# Inspecting the returned dada-class object
dadaFs[1]
dadaRs[6]



# ________________ Merge paired reads _______________

mergers <- mergePairs(dadaFs, derepFs, dadaRs, derepRs, verbose=TRUE)
head(mergers[[1]])
# merged together the inferred forward and reverse sequences and eliminated the residual errors 
# (non-overlap paired sequences) to construct the ASVs table (ASVs and their abundances).



# ________________ Construct ASV table _______________

seqtab <- makeSequenceTable(mergers)
sum(seqtab) 
dim(seqtab)

class(seqtab) # matrix, array

# remove chimera
seqtab.nochim <- removeBimeraDenovo(seqtab, method = "consensus", 
                                    multithread = T, verbose = T) 
# Identified XXX bimeras out of YYYY input sequences.
sum(seqtab.nochim) 
dim(seqtab.nochim) 

# Calculate the percentage of remained sequences after removing chimera
sum(seqtab.nochim)/sum(seqtab) #0.9058601


#_____________ TRACK THE NUMBER OF READS through the pipeline _______________

getN <- function(x) sum(getUniques(x))
track <- cbind(out, 
               sapply(dadaFs, getN), 
               sapply(dadaRs, getN), 
               sapply(mergers, getN), 
               rowSums(seqtab.nochim))
# If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
rownames(track) <- sample.names
head(track)


# ____________ Taxanomy assignment ____________

# Download training data set at https://zenodo.org/record/4587955#.YlecbchBy00
# Assign taxonomy to genus level
taxaRC <- assignTaxonomy(seqtab.nochim, 
                         "/Volumes/T7/archaea/database/silva_nr99_v138.1_train_set.fa",
                         tryRC=T, multithread = T) 

# Assign taxonomy to species level
taxaSp <- addSpecies(taxaRC, "/Volumes/T7/archaea/database/silva_species_assignment_v138.1.fa")  

taxaRC_print = taxaRC
rownames(taxaRC_print) = NULL
taxaSp_print = taxaSp
rownames(taxaSp_print) = NULL
taxaRC[is.na(taxaRC)] <- "Unclassified"

class(taxaRC) # matrix, array
taxaRC_print = as.data.frame(taxaRC_print)
taxaSp_print = as.data.frame(taxaSp_print)
taxaRC_print[is.na(taxaRC_print)] <- "Unclassified"

# Inspecting the taxaSp_print -> There are 2 kingdoms: Bacteria and Archaea
# From this step, we performed analysis for these two kingdoms individually
# Function to count unclassified ASVs
count_unclassified <- function(x) {sum(x == "Unclassified")}

# Summary for unclassified ASVs
specs.clean <- specs[, -c(1,2,3,4,5)]
bacteria_summary.unc <- sapply(specs.clean[specs.clean$Kingdom == "Bacteria", ], count_unclassified)
archaea_summary.unc <- sapply(specs.clean[specs.clean$Kingdom == "Archaea", ], count_unclassified)
summary_df.unc <- data.frame(Bacteria = bacteria_summary.unc, Archaea = archaea_summary.unc)
View(summary_df.unc)

# Summary for identidied ASVs
bacteria_summary <- sapply(specs.clean[specs.clean$Kingdom == "Bacteria", ], function(x) length(unique(x)))
archaea_summary <- sapply(specs.clean[specs.clean$Kingdom == "Archaea", ], function(x) length(unique(x)))
summary_df <- data.frame(Bacteria = bacteria_summary, Archaea = archaea_summary)
View(summary_df)


# ____________ Microbiome analysis ____________

samples.out <- rownames(seqtab.nochim)
samples.out

samples.out.clean <-rownames(seqtab.nochim[-c(3, 8, 9),])
samples.out.clean

subject <- substr(samples.out.clean,1,2)
sampledf <- data.frame(Subject=subject)
sampledf$Location[sampledf$Subject == "AF" | sampledf$Subject == "BR" | sampledf$Subject == "AM"] <- "Corals"
sampledf$Location[sampledf$Subject == "SD"] <- "Sediment"
sampledf$Location[sampledf$Subject == "WC"] <- "Water column"
rownames(sampledf) <- samples.out.clean

seqtab.nochim <- as.matrix(t(as.data.frame(seqtab.nochim)))
ps <- phyloseq(otu_table(seqtab.nochim, taxa_are_rows = T),
               tax_table(taxaSp), sample_data(sampledf))

ntaxa(ps)
melt <- psmelt(ps)

# Save ps0 as the original phyloseq object -> further changes should be made to 
# new copies of this object to avoid altering the original/ untouched data
ps0 <- ps
tax.clean <- data.frame(tax_table(ps))
for (i in 1:7){ tax.clean[,i] <- as.character(tax.clean[,i])}
tax.clean[is.na(tax.clean)] <- "Unclassified"
tax_table(ps) <- as.matrix(tax.clean)

sam.clean <- data.frame(sample_data(ps))


# Getting dataframes that summarize the composition at different levels:
king <- ps%>%
  tax_glom(taxrank = "Kingdom")%>% # agglomerate data of the same phylum
  transform_sample_counts(function(x) {x/sum(x)})%>% 
  psmelt()%>% # combine format(?), melt and merge all into a dataframe
  filter(Abundance > 0.002)%>%
  arrange(Kingdom) # arrange dataframe alphabetically by phylum

phy <- ps%>%
  tax_glom(taxrank = "Phylum")%>% # agglomerate data of the same phylum
  transform_sample_counts(function(x) {x/sum(x)})%>% 
  psmelt()%>% # combine format(?), melt and merge all into a dataframe
  filter(Abundance > 0.002)%>%
  arrange(Phylum) # arrange dataframe alphabetically by phylum

class = ps0%>%
  tax_glom(taxrank = "Class")%>%
  transform_sample_counts(function(x) {x/sum(x)})%>%
  psmelt()%>%
  filter(Abundance > 0.002)%>%
  arrange(Class)

order = ps%>%
  tax_glom(taxrank = "Order")%>%
  transform_sample_counts(function(x) {x/sum(x)})%>%
  psmelt()%>%
  filter(Abundance > 0.002)%>%
  arrange(Order)

fam = ps%>%
  tax_glom(taxrank = "Family")%>%
  transform_sample_counts(function(x) {x/sum(x)})%>%
  psmelt()%>%
  filter(Abundance > 0.002)%>%
  arrange(Family)

genus = ps0%>%
  tax_glom(taxrank = "Genus")%>%
  transform_sample_counts(function(x) {x/sum(x)})%>%
  psmelt()%>%
  filter(Abundance > 0.002)%>%
  arrange(Genus)

specs = ps%>%
  tax_glom(taxrank = "Species")%>%
  transform_sample_counts(function(x) {x/sum(x)})%>%
  psmelt()%>%
  filter(Abundance > 0.002)%>%
  arrange(Species)


# Normalize the abundance for better visualization:

# BACTERIA
backing100qt <- king%>%
  group_by(Sample)%>%
  filter(Kingdom == "Bacteria")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

bacgenus100qt <- genus%>%
  group_by(Sample)%>%
  filter(Kingdom == "Bacteria")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)
  
bacphy100qt <- phy%>%
  group_by(Sample)%>%
  filter(Kingdom == "Bacteria")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

bacclass100qt <- class%>%
  group_by(Sample)%>%
  filter(Kingdom == "Bacteria")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

bacorder100qt <- order%>%
  group_by(Sample)%>%
  filter(Kingdom == "Bacteria")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

bacfam100qt <- fam%>%
  group_by(Sample)%>%
  filter(Kingdom == "Bacteria")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

# ARCHAEA:
king100qt <- king%>%
  group_by(Sample)%>%
  #filter(Kingdom == "Archaea")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

archphy100qt <- phy%>%
  group_by(Sample)%>%
  filter(Kingdom == "Archaea")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

archgenus100qt <- genus%>%
  group_by(Sample)%>%
  filter(Kingdom == "Archaea")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

archclass100qt <- class%>%
  group_by(Sample)%>%
  filter(Kingdom == "Archaea")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

archorder100qt <- order%>%
  group_by(Sample)%>%
  filter(Kingdom == "Archaea")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)

archfam100qt <- fam%>%
  group_by(Sample)%>%
  filter(Kingdom == "Archaea")%>%
  mutate(Abundance = Abundance / sum(Abundance) * 1)
