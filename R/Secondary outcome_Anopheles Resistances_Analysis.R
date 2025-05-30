# secondary outcomes analysis (entomological outcomes)
## Insecticide mutations (kdr-w, kdr-e & ace-1) Analyses
### Load required packages
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(boot)

### functions used to calculate bootstrap CI of mean HBR, SR and EIR
samplemean <- function(x, d) {
	return(mean(x[d], na.rm=TRUE))
}

ci <- function(x){
	boot_hbr <- boot(x, samplemean, 1000)
	boot_ci <- boot.ci(boot_hbr, type = "perc")
	return(boot_ci$perc)
}

### Load data
load("Data/entomo.RData")

# KDR-W ----
## BF
### model
glm_KDRW_BF <- glmmTMB(fa ~ prepost + prepost:intervention + pcr_espece +(1|nummission) + (1|codevillage/idmoustique), data = KDRW_BF, family = binomial(link = "logit"))

### statistics
count_KDRW_BF <- KDRW_BF %>%
	group_by(intervention, prepost) %>%
	summarise(n= sum(fa, na.rm=TRUE),N=n(), m = mean(fa,na.rm=TRUE), ci_low = ci(fa)[4], ci_hig = ci(fa)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

### comparisons
OR_KDRW_BF <- emmeans(glm_KDRW_BF , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL, 2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = F)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

### results (Supplementary Table 21)
bind_cols(count_KDRW_BF, OR_KDRW_BF)

## CI
### model
glm_KDRW_CI <- glmmTMB(fa ~ intervention + (1|nummission) + (1|codevillage/idmoustique), data = KDRW_CI, family = binomial(link = "logit"))

### statistics
count_KDRW_CI <- KDRW_CI %>%
	group_by(intervention) %>%
	summarise(n= sum(fa, na.rm=TRUE),N=n(), m = mean(fa,na.rm=TRUE), ci_low = ci(fa)[4], ci_hig = ci(fa)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N))

### comparisons
OR_KDRW_CI <- emmeans(glm_KDRW_CI , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle", infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL, 2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = F)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

### results (Supplementary Table 21)
bind_cols(count_KDRW_CI, OR_KDRW_CI)

# KDR-E ----
## BF
### model
glm_KDRE_BF <- glmmTMB(fa ~prepost + prepost:intervention + pcr_espece + (1|nummission) + (1|codevillage/idmoustique), data = KDRE_BF, family = binomial(link = "logit"))

### statistics
count_KDRE_BF <- KDRE_BF %>%
	group_by(intervention, prepost) %>%
	summarise(n= sum(fa, na.rm=TRUE),N=n(), m = mean(fa,na.rm=TRUE), ci_low = ci(fa)[4], ci_hig = ci(fa)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

### comparisons
OR_KDRE_BF <- emmeans(glm_KDRE_BF , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL, 2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = F)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

### results (Supplementary Table 21)
bind_cols(count_KDRE_BF, OR_KDRE_BF)

# ACE1 ----
## BF
### model
glm_ACE1_BF <- glmmTMB(fa ~prepost + prepost:intervention + (1|nummission) + (1|codevillage/idmoustique), data = ACE1_BF, family = binomial(link = "logit"))

### statistics
count_ACE1_BF <- ACE1_BF %>%
	group_by(intervention, prepost) %>%
	summarise(n= sum(fa, na.rm=TRUE),N=n(), m = mean(fa,na.rm=TRUE), ci_low = ci(fa)[4], ci_hig = ci(fa)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

### comparisons
OR_ACE1_BF <- emmeans(glm_ACE1_BF , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL, 2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = F)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

### results (Supplementary Table 21)
bind_cols(count_ACE1_BF, OR_ACE1_BF)

## CI
### model
glm_ACE1_CI <- glmmTMB(fa ~ intervention + (1|nummission) + (1|codevillage/idmoustique), data = ACE1_CI, family = binomial(link = "logit"))

### statistics
count_ACE1_CI <- ACE1_CI %>%
	group_by(intervention) %>%
	summarise(n= sum(fa, na.rm=TRUE),N=n(), m = mean(fa,na.rm=TRUE), ci_low = ci(fa)[4], ci_hig = ci(fa)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N))

### comparisons
OR_ACE1_CI <- emmeans(glm_ACE1_CI , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle", infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL, 2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = F)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

### results (Supplementary Table 21)
bind_cols(count_ACE1_CI, OR_ACE1_CI)
