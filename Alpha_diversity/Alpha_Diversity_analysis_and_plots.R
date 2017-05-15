# Alpha Diversity Analysis for Torres et al. 2017 Developmental Stage Influences metabolic and gut microbiome of PCOS mouse model
# Used the output from qiime using alpha_diversity.py followed by add_alpha_to_mapping_file
getwd()
#setwd("/Volumes/PBD/PCOS.PhD/Microbiome_Torres.et.al.Files")

setwd("/Users/Pedro_Torres/Desktop/PCOS_Analysis/PCOS.16S.PhD/Age_study_not_merged")
#import mapping file with diversity metrics
mapping_file=read.csv("mapping_file_kina_dropped_samples_alphaDiv.csv", header=T, sep=",", as.is = T, row.names = 1, check.names = F )
#labels(mapping_file)

# alpha diverity

#placebo-------
placebo=subset(mapping_file, Treatment =="Placebo")

#PD
lm.pd.p=with(placebo, lm(PD_whole_tree_alpha~time))
summary(lm.pd.p)
with(placebo,plot(time,PD_whole_tree_alpha, title("Placebo"), xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Faith's PD)"))
abline(lm.pd.p)

#chao1
lm.chao1.p=with(placebo, lm(chao1_alpha~time))
summary(lm.chao1.p)
with(placebo, plot(time,chao1_alpha,title("Placebo"), xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Chao1)"))
abline(lm.chao1.p)

#equitability-eveness
lm.equitability=with(placebo, lm(equitability~time))
summary(lm.equitability)
with(placebo, plot(time,equitability,title("Placebo"),xlab="Time Post-Treatment (Weeks)", ylab="Eveness (Equitibilaty)"))
abline(lm.equitability)

#letrozole----------------
letrozole=subset(mapping_file, Treatment =="Letrozole")
unique(letrozole)

#PD
lm.pd=with(letrozole, lm(PD_whole_tree_alpha~time))
summary(lm.pd)
with(letrozole,plot(time,PD_whole_tree_alpha,title("Letrozole"), xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Faith's PD)"))
abline(lm.pd)

#chao1
lm.chao1=with(letrozole, lm(chao1_alpha~time))
summary(lm.chao1)
with(letrozole, plot(time,chao1_alpha,title("Letrozole"),xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Chao1)"))
abline(lm.chao1)

#equitability
lm.equitability.L=with(letrozole, lm(equitability~time))
summary(lm.equitability.L)
with(letrozole, plot(time,equitability,title("Letrozole"),xlab="Time Post-Treatment (Weeks)", ylab="Eveness (Equitibility)"))
abline(lm.equitability.L)







# Supplementary Material and more ________________________________________



#placebo supplementary material
#shannon
lm.shannon.p=with(placebo, lm(shannon_alpha~time))
summary(lm.shannon.p)
with(placebo,plot(time, shannon_alpha, title("Placebo"), xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Shannon)"))
abline(lm.shannon.p)
#observedotus.
lm.observed.otus.p=with(placebo, lm(observed_otus_alpha~time))
summary(lm.observed.otus.p)
with(placebo, plot(time, observed_otus_alpha, title("Placebo"), xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Observed OTUs)"))
abline(lm.observed.otus.p)


#letrozole
#shannon
lm.shannon=with(letrozole, lm(shannon_alpha~time))
summary(lm.shannon)
with(letrozole, plot(time,shannon_alpha, title("Letrozole"),xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Shannon)"))
abline(lm.shannon)
#observed otus
lm.observed_otus=with(letrozole, lm(observed_otus_alpha~time))
summary(lm.observed_otus)
with(letrozole,plot(time, observed_otus_alpha, title("Letrozole"),xlab="Time Post-Treatment (Weeks)", ylab="Diversity (Observed OTUs)"))
abline(lm.observed_otus)
letrozole$observed_otus_alpha
placebo=subset(mapping_file, Treatment =="Placebo")
unique(placebo)
lm.pd=with(placebo, lm(PD_whole_tree_alpha~time))
summary(lm.pd)
with(placebo,plot(time,PD_whole_tree_alpha))
abline(lm.pd)

