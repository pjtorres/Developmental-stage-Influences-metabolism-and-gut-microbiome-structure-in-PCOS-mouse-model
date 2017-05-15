#Dataset used for Torres et al. 2017 Developmental stage influences metabolic and gut microbiome profile in PCOS mouse model. After doing group_significance.py in qiime the bacterial genera that had a p>0.05 (after FDR correcting) in the adult model and seperately in the pubertal model were copied from /4week_8week_merged/taxa_plots/merged_sorted_L6.txt and pasted into a new file called baccounts.csv and then this was used to create a heatmap for both develpomental stages.
#install.packages("pheatmap")
library(pheatmap)
library(dplyr)
getwd()
setwd("/Users/Pedro_Torres/Desktop/PCOS_Analysis/PCOS.16S.PhD/4weekand8week/heatmap/heatmap8weekonly/")

#Looking at week 8 (adult) mouse model only------------------------
Bac.counts <- read.csv(file = "baccounts8week.csv", header = TRUE, row.names=1,check.names=FALSE)
Bac.factors <- read.csv(file = "bacfactor8weeks.csv", header = TRUE, row.names=1,check.names=FALSE)

#heatmap
pheatmap(Bac.counts)


#colour sample groups
Bac.factorsDS <- select(Bac.factors, Treatment_study, Treatment)
pheatmap(Bac.counts, annotation_col = Bac.factorsDS)

# Reorder Density levels to Sparse, Dense, Other
Bac.factorsDS$Treatment_study = factor(Bac.factorsDS$Treatment_study, levels = c("8wk.P", "8wk.L"))
DensityCol <- c("darkorchid", "red")
names(DensityCol) <- levels(Bac.factorsDS$Treatment_study)

# Reorder TREATMENT to placebo and letrozole
Bac.factorsDS$Treatment <- factor(Bac.factorsDS$Treatment, levels = c("p", "l"))

SpeciesCol <- c("forestgreen", "blue3")
names(SpeciesCol) <- levels(Bac.factorsDS$Treatment)

# Add to a list, where names match those in factors dataframe
AnnColour = list(Treatment_study = DensityCol,Treatment = SpeciesCol)

# Check the output
AnnColour
#redraw heatmap
pheatmap(Bac.counts, annotation_col = Bac.factorsDS, annotation_colors = AnnColour)

pheatmap(Bac.counts, clustering_distance_rows = "manhattan",
         clustering_distance_cols = "manhattan", clustering_method = 'average',
         annotation_colors = AnnColour, annotation_col = Bac.factorsDS)

#scaling variables-We can now see how many standard deviations the Log10 abundance of a single OTU is away from the mean for that OTU in a sample compared only with other samples for that OTU
pheatmap(Bac.counts, scale = "row", clustering_distance_rows = "manhattan",
         clustering_method = 'average',
         annotation_colors = AnnColour, annotation_col = Bac.factorsDS)
#sort by groups
SampleOrder = order(Bac.factorsDS$Treatment, Bac.factorsDS$Treatment_study)

pheatmap(Bac.counts[ , SampleOrder], cluster_cols = FALSE,
         clustering_method = 'average', annotation_colors = AnnColour, annotation_col = Bac.factorsDS)

#looking at 4 week (pubertal) mouse model only-----------------

setwd("/Users/Pedro_Torres/Desktop/PCOS_Analysis/PCOS.16S.PhD/4weekand8week/heatmap/heatmap4weekonly/")

Bac.counts <- read.csv(file = "bacterialcounts.csv", header = TRUE, row.names=1,check.names=FALSE)
Bac.factors <- read.csv(file = "bacterialfactors.csv", header = TRUE, row.names=1,check.names=FALSE)

#heatmap
pheatmap(Bac.counts)


#colour sample groups
Bac.factorsDS <- select(Bac.factors, Treatment_study, Treatment)
pheatmap(Bac.counts, annotation_col = Bac.factorsDS)


# Reorder Density levels to Sparse, Dense, Other
Bac.factorsDS$Treatment_study = factor(Bac.factorsDS$Treatment_study, levels = c( "4wk.l","4wk.p"))
DensityCol <- c("darkorchid", "red")
names(DensityCol) <- levels(Bac.factorsDS$Treatment_study)

# Reorder TREATMENT to placebo and letrozole
Bac.factorsDS$Treatment <- factor(Bac.factorsDS$Treatment, levels = c("p", "l"))

SpeciesCol <- c("forestgreen", "blue3")
names(SpeciesCol) <- levels(Bac.factorsDS$Treatment)

# Add to a list, where names match those in factors dataframe
AnnColour = list(Treatment_study = DensityCol,Treatment = SpeciesCol)

# Check the output
AnnColour
#redraw heatmap
pheatmap(Bac.counts, annotation_col = Bac.factorsDS, annotation_colors = AnnColour)

pheatmap(Bac.counts, clustering_distance_rows = "manhattan",
         clustering_distance_cols = "manhattan", clustering_method = 'average',
         annotation_colors = AnnColour, annotation_col = Bac.factorsDS)

#scaling variables-We can now see how many standard deviations the Log10 abundance of a single OTU is away from the mean for that OTU in a sample compared only with other samples for that OTU
pheatmap(Bac.counts, scale = "row", clustering_distance_rows = "manhattan",
         clustering_method = 'average',
         annotation_colors = AnnColour, annotation_col = Bac.factorsDS)
#sort by groups
SampleOrder = order(Bac.factorsDS$Treatment, Bac.factorsDS$Treatment_study)

pheatmap(Bac.counts[ , SampleOrder], cluster_cols = FALSE,
         clustering_method = 'average', annotation_colors = AnnColour, annotation_col = Bac.factorsDS)

