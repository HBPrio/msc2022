#!/usr/bin/env Rscript

library(readr)
library(ggplot2)
library(nortest, pos=17) # ad.test
library(pgirmess) # kruskalmc
library(agricolae) # kruskal with tukey groups
library(RColorBrewer)
library(rcompanion)  # multiVDA
library(effsize) # VD.A
library(dplyr) # glimpse

#==================================================
# Preparation
#==================================================
setwd("c:/Users/siqueira/Documents/...")

approaches <- c(
  "Optimal",
  "HB-all",
  "HB-fam",
  "RealOrd",
  "NewOld",
  "Rnd",
  "hbd_rnd_t80",
  "hbd_ncs_t80"
)
v_factor_levels <- unique(approaches)

#LOAD RESULTS FILE
results_loc <- "apfd.csv"
raw_results <- read_delim(results_loc, ";", escape_double = FALSE, trim_ws = TRUE)

raw_results <- subset(raw_results, Approach %in% approaches)

pdf_w <- 9
pdf_h <- 6
my.cols = brewer.pal(n = 8, name = "Dark2")

#reordering and renaming
raw_results$Approach <- factor(raw_results$Approach, 
                               levels=c("Optimal","HB-fam","HB-all","hbd_ncs_t80","hbd_rnd_t80","NewOld","RealOrd","Rnd"),
                               labels=c("Optimal","Fam-dep","Fam-ind","HBD","HBR","NewOld","RealOrd","Random"),)


#==================================================
# Boxplots (TTFF)
#==================================================
pdf_w <- 16
pdf_h <- 12
my.cols = brewer.pal(n = 8, name = "Dark2")

pdf("APFD_BoxPlot.pdf", width=pdf_w, height=pdf_h)

ggplot(data=subset(raw_results, !is.na(APFD)), aes(Approach, APFD, fill=Approach))+ 
  geom_boxplot(outlier.size = 3, size=1)+
  scale_fill_manual(values = c("#F37B59", "#A3A500", "#00BE6C","#00C0B8", "#00B8E5", "#00A5FF","#9590FF","#DC71FA"))+
  theme_light()+ 
  theme(axis.text = element_text(size = 18),
                           axis.title.y = element_text(size = 22, margin = margin(r=10)),
                           legend.position="none",
                           axis.title.x = element_blank())

dev.off()

#==================================================
# Normality Test (APFD)
#==================================================
#NORMALITY TEST (AD)
#The null hypothesis for the A-D test is that the data does follow a normal distribution. 
#If p-value < significance level, the data does not follow a normal distribution.
with(raw_results, ad.test(APFD))

#==================================================
# APFD
#==================================================
#Kruskal-Wallis rank sum test
with(raw_results, tapply(APFD, Approach, median, na.rm=TRUE))
kruskal.test(APFD ~ Approach, data=raw_results)

#Kruskal-Wallis rank sum test (MULTIPLE COMPARISON)
kruskalmc(raw_results$APFD, raw_results$Approach)

# Pairwise comparisons using Wilcoxon’s test
#wilcox_test(raw_results, APFD ~ Approach, p.adjust.method = "bonferroni")

out <- kruskal(raw_results$APFD, raw_results$Approach)
out

# ?rcompanion::multiVDA

#==================================================
# Pairwise Vargha and Delaney's A and Cliff's delta
#==================================================
multiVDA(APFD ~ Approach, data=raw_results)

#==================================================
# VD.A: Vargha and Delaney A measure 
# (small, >= 0.56; medium, >= 0.64; large, >= 0.71)
#==================================================