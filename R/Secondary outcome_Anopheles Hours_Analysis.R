# secondary outcomes analysis (entomological outcomes)
## Hour of collection - Analyses
### Load required packages
library(tidyverse)
library(rstatix)

### functions used to transform hours data 
#### change referential
hour_to_n <- function(x){
	if (x > 12) {n <- x - 12} else {n <- x + 12 }
	return(n)
}

#### vectorise the latter
Vhour_to_n <- Vectorize(hour_to_n)

#### rescale between 0 and 1 and inverse
n_to_scT <- function(n, min_n, max_n){
	scT <- (n-min_n)/(max_n-min_n)
	return(scT)
}

scT_to_n <- function(x, min_n, max_n){
	n <- x*(max_n-min_n)+min_n
	return(n)
}


### Load data
load("Data/entomo.RData")

### BF ----
#### compare median time between interventions
Dunn_Hour_BF <- Hour_BF %>%
	filter(prepost == 1) %>%
	dunn_test(hour~intervention, detailed = T) %>%
	slice(-3) %>%
	select(estimate,p.adj) %>%
	add_row(.before = 1)

#### statitics (pour Dunn's test : median et IQR)
count_Hour_BF <- Hour_BF %>%
	filter(prepost == 1) %>%
	group_by(intervention) %>%
	summarise(N=n(), m = quantile(hour, 0.5), ci_low = quantile(hour, 0.25), ci_hig = quantile(hour, 0.75)) %>%
	mutate(across(c(m,ci_low,ci_hig),Vhour_to_n)) %>%
	mutate(median = paste0(m," [",ci_low,";",ci_hig, "] (",N,")")) %>%
	select(-c(m,ci_low,ci_hig,N)) 

#### results (Supplementary Table 22)
bind_cols(count_Hour_BF, Dunn_Hour_BF)

### CIV ----
#### compare median time between interventions
Dunn_Hour_CI <- Hour_CI %>%
	dunn_test(hour~intervention, detailed = TRUE) %>%
	slice(-3) %>%
	select(estimate,p.adj) %>%
	add_row(.before = 1)

#### statistics (pour Dunn's test : median et IQR)
count_Hour_CI <- Hour_CI %>%
	group_by(intervention) %>%
	summarise(N=n(), m = quantile(hour)[3], ci_low = quantile(hour)[2], ci_hig = quantile(hour)[4]) %>%
	mutate(across(c(m,ci_low,ci_hig),Vhour_to_n)) %>%
	mutate(median = paste0(m," [",ci_low,";",ci_hig, "] (",N,")")) %>%
	select(-c(m,ci_low,ci_hig,N)) 

#### results (Supplementary Table 22)
bind_cols(count_Hour_CI, Dunn_Hour_CI)
