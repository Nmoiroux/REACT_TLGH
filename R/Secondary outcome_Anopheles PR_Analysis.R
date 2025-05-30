# secondary outcomes analysis (entomological outcomes)
## Anopheles Parity Rate
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

# Parity analysis ----
## BF
#### count data
count_PR_BF <- PR_BF %>%
	group_by(intervention, prepost) %>%
	summarise(n= sum(pr, na.rm=TRUE),N=n(), m = mean(pr,na.rm=TRUE), ci_low = ci(pr)[4], ci_hig = ci(pr)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### model
glm_PR_BF <- glmmTMB(pr ~prepost + prepost:intervention + (1|nummission) + (1|codevillage/point/poste), data = PR_BF, family = binomial(link = "logit"))

#### comparison
OR_PR_BF <- emmeans(glm_PR_BF , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### results (Supplementary Table 20)
res_PR_BF <- bind_cols(count_PR_BF, OR_PR_BF)


## CI
#### count data
count_PR_CI <- PR_CI %>%
	group_by(intervention) %>%
	summarise(n= sum(pr, na.rm=TRUE),N=n(), m = mean(pr,na.rm=TRUE), ci_low = ci(pr)[4], ci_hig = ci(pr)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) 

#### model
glm_PR_CI <- glmmTMB(pr ~ intervention + (1|nummission) + (1|codevillage/point/poste), data = PR_CI, family = binomial(link = "logit"))

#### comparison
OR_PR_CI <- emmeans(glm_PR_CI , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle", infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### results (Supplementary Table 20)
res_PR_CI <- bind_cols(count_PR_CI, OR_PR_CI)
