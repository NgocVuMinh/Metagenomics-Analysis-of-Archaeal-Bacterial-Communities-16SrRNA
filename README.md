# Metagnemonic characterizations of archaea and bacteria associated with coral reef-ecosystem in Phu Quoc Islands, Vietnam

### Contributor: [Ngoc M. Vu](https://github.com/NgocVuMinh)
This repository contains R scripts used to process 16S rRNA sequencing data to analyse the composition and diversity of archaea in relation with the bacterial community, both associated with a coral reef system in Phu Quoc Islands, Vietnam. 

Samples were collected from three biotopes: corals (Acropora sp. and Lobophyllia sp. a.k.a. brain corals), sediment surrounding the coral reefs, and water columns above the reefs. 16 samples were collected in total. 16S rRNA sequencing was performed using Illumina Hiseq.

This repo is part of the study: [Phuong Thao, N. T., Minh Ngoc, V., Tra, P. V., & Bui Van. (2023). Metagenomic characterization of archaeal and bacterial communities associated with coral, sediment, and seawater in a coral reef ecosystem of Phu Quoc island, Vietnam . Vietnam Journal of Biotechnology, 21(4), 745–757.](https://doi.org/10.15625/1811-4989/20283)

## 16S rRNA sequencing data
The data in FASTQ format will be made available upon reasonable requests. The original data was provided by the Department of Bioinformatics, Institute of Biotechnology, Vietnam Academy of Science and Technology (VAST).

The scripts could be used to process any 16S rRNA data.


## Analysis pipeline and libraries

The DADA2 and phyloseq pipeline were used for this analysis. Details could be found at:
* DADA2: https://benjjneb.github.io/dada2/tutorial_1_8.html
* Phyloseq: https://www.nicholas-ollberding.com/post/introduction-to-phyloseq/

Overview of steps in the pipeline:
* Reading FASTQ files and assessing the quality of sequencing reads
* Filtering and trimmming reads
* DADA2: (1) Learn the error rates, (2) dereplicating, (3) creating dada objects, (4) merging forward and reverse reads, (5) constructing ASV tables, (6) removing chimera, (7) taxonomic assignment
* phyloseq: manipulating taxonomic composition for visualization and comparison
* alpha and beta diversity analysis

R packages and libraries used:

```
library(tidyverse)
library(phyloseq)
library(dada2)
library(Rcpp)
library(ggplot2)
library(gplots)
library(RColorBrewer)
library(ggforce)
library(gridExtra)
library(grid)
library(dplyr)
library(tidyr)
library(Heatplus)
library(vegan)
library(ggforce)
library(ggsignif)
library(ggpubr)
library(microViz)
```

All scripts were run in R-Studio, R version 4.3.1


## Usage

* Installing the required libraries and packages

* Running the following scripts in R studio:

```
main.R # DADA2 + phyloseq
color_palettes.R # a multi-purpose script to get colors for visualization
visualization.R # visualize bacterial and archaeal composition 
alpha_analysis.R 
beta_analysis.R
```