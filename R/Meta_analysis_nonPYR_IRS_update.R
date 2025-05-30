# updated meta-analysis (non-PYR IRS)
## Load required packages
library(tidyverse)
library(metafor)

## Load data
load("Data/data_meta_IRS.RData")

## reproducing Pryde et al.'s meta-analysis (2022)
data_meta_IRS %>% 
	filter(!(Study %in% c("MoirouxBF","MoirouxCI","Moiroux"))) %>%
	rma(yi=estimate, sei = se, data=.,method = "DL", slab = Study) %>%
	forest.rma(transf=exp, header = "Study", at=c(0.1, 1, 2))

## updated meta-analyses
meta_updated <- data_meta_IRS %>% 
	filter(!(Study %in% c("MoirouxBF","MoirouxCI"))) %>% 
	#filter(!(Study %in% c("Moiroux"))) %>% # meta-analysis spiting Moiroux et al. results by country
	rma(yi=estimate, sei = se, data=.,method = "DL", slab = Study)

### summary of analysis (Supplementary Figure 2)
meta_updated

### Supplementary Table 5
data_meta_IRS %>% 
	filter(!(Study %in% c("MoirouxBF","MoirouxCI"))) %>%
	mutate(weights = weights(meta_updated))

### plot rma (Supplementary Figure 3)
meta_updated %>%
	forest.rma(transf=exp, header = "Study", at=c(0.1, 1, 2))
