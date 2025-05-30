# secondary outcomes analysis (entomological outcomes)
## Anopheles Sporozoite rate
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


### Analaysis (BF area)
#### count data
count_SR_BF <- SR_BF %>%
	group_by(intervention, prepost) %>%
	summarise(n= sum(pcr_pf, na.rm=TRUE),N=n(), m = mean(pcr_pf,na.rm=TRUE), ci_low = ci(pcr_pf)[4], ci_hig = ci(pcr_pf)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### model
glm_SR_BF <- glmmTMB(pcr_pf~prepost + prepost:intervention + (1|nummission) + (1|codevillage/point/poste), data = SR_BF, family = binomial(link = "logit"))

#### comparison
OR_SR_BF <- emmeans(glm_SR_BF , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### results (Table 5)
res_SR_BF <- bind_cols(count_SR_BF, OR_SR_BF)


### Analaysis (CI area)

#### count summary
count_SR_CI <- SR_CI %>%
	group_by(intervention) %>%
	summarise(n= sum(pcr_pf, na.rm=TRUE),N=n(), m = mean(pcr_pf,na.rm=TRUE), ci_low = ci(pcr_pf)[4], ci_hig = ci(pcr_pf)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	rename(postintervention = mean)

#### model
glm_SR_CI <- glmmTMB(pcr_pf~ intervention + (1|nummission) + (1|codevillage/point), data = SR_CI, family = binomial(link = "logit"))

#### comparison
OR_SR_CI <- emmeans(glm_SR_CI , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle", infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### results (Table 5)
res_SR_CI <- bind_cols(count_SR_CI, OR_SR_CI)

