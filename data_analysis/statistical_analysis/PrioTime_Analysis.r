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
  "HB-all",
  "HB-fam",
  "hbd_rnd_t80",
  "hbd_ncs_t80"
)
v_factor_levels <- unique(approaches)

#LOAD RESULTS FILE
results_loc <- "time.csv"
raw_results <- read_delim(results_loc, ",", escape_double = FALSE, trim_ws = TRUE)
raw_results <- subset(raw_results, Approach %in% approaches)

pdf_w <- 9
pdf_h <- 6
my.cols = brewer.pal(n = 8, name = "Dark2")

#reordering and renaming
raw_results$Approach <- factor(raw_results$Approach, 
                               levels=c("HB-all","HB-fam","hbd_rnd_t80","hbd_ncs_t80"),
                               labels=c("Fam-ind","Fam-dep","HBR","HBD"),)

#==================================================
# Boxplots (TTFF)
#==================================================
pdf("PrioTime_BoxPlot.pdf", width=pdf_w, height=4)

ggplot(data=subset(raw_results), aes(Approach, Prio_Time, fill=Approach))+ 
  geom_boxplot(width=0.4, outlier.size = 2, size=1) +
  coord_flip()+
  scale_fill_manual(values = c("#00BE6C", "#A3A500", "#00BE6C", "#00B8E5"))+
  labs(y="Prioritization Time (sec)")+
  theme_light()+ 
  theme(axis.text = element_text(size = 18),
                           axis.title.y = element_text(size = 22, margin = margin(r=10)),
                           axis.title.x = element_text(size = 22, margin = margin(t=10)),
                           legend.position="none")

dev.off()

#==================================================
# Normality Test (APFD)
#==================================================
#NORMALITY TEST (AD)
#The null hypothesis for the A-D test is that the data does follow a normal distribution. 
#If p-value < significance level, the data does not follow a normal distribution.
with(raw_results, ad.test(Prio_Time))

#==================================================
# APFD
#==================================================
#Kruskal-Wallis rank sum test
with(raw_results, tapply(Prio_Time, Approach, median, na.rm=TRUE))
kruskal.test(Prio_Time ~ Approach, data=raw_results)

#Kruskal-Wallis rank sum test (MULTIPLE COMPARISON)
kruskalmc(raw_results$Prio_Time, raw_results$Approach)

# Pairwise comparisons using Wilcoxon’s test
#wilcox_test(raw_results, APFD ~ Approach, p.adjust.method = "bonferroni")

out <- kruskal(raw_results$Prio_Time, raw_results$Approach)
out

#==================================================
# Pairwise Vargha and Delaney's A and Cliff's delta
#==================================================
multiVDA(Prio_Time ~ Approach, data=raw_results)

#==================================================
# VD.A: Vargha and Delaney A measure 
# (small, >= 0.56; medium, >= 0.64; large, >= 0.71)
#==================================================