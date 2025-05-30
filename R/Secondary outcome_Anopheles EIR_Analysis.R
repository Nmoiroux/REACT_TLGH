# secondary outcomes analysis (entomological outcomes)
## Anopheles Entomological Incolulation Rate
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


# EIR Analysis ----
## EIR analysis (BF)

#### model
glmm_eir_BF <- glmmTMB(eir ~ prepost + prepost:intervention + (1|nummission) + (1|codevillage/point/poste), data = eir_BF, family = nbinom2(link = "log"))

#### comparison
RR_eir_BF <- emmeans(glmm_eir_BF , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

#### count data 
count_eir_BF <- eir_BF %>%
	group_by(intervention, prepost) %>%
	summarise(n=sum(eir), d= n(), m = mean(eir), ci_low = ci(eir)[4], ci_hig = ci(eir)[5]) %>%
	mutate(mean = paste0(round(m,2),"[",round(ci_low,2),";",round(ci_hig,2), "] (",n,"/",d,")")) %>%
	select(-c(m,ci_low,ci_hig,n,d)) %>%
	pivot_wider(names_from = prepost, values_from = c("mean")) %>%
	rename(preintervention = "0", postintervention = "1")

#### results (Table 5)
res_eir_BF <- bind_cols(count_eir_BF, RR_eir_BF)


## EIR analysis (CI)
#### model
glmm_eir_CI <- glmmTMB(r_eir ~ intervention + (1|nummission) + (1|codevillage/point/poste), data = eir_CI, family = nbinom2(link = "log"))

#### comparison
RR_eir_CI <- emmeans(glmm_eir_CI , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle", infer=TRUE) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

#### count data 
count_eir_CI <- eir_CI %>%
	group_by(intervention) %>%
	summarise(n=sum(r_eir, na.rm = T), d= n(), m = mean(r_eir, na.rm = T), ci_low = ci(r_eir)[4], ci_hig = ci(eir)[5]) %>%
	mutate(mean = paste0(round(m,2),"[",round(ci_low,2),";",round(ci_hig,2), "] (",n,"/",d,")")) %>%
	select(-c(m,ci_low,ci_hig,n,d)) %>%
	rename(postintervention = mean)

#### results (Table 5)
res_eir_CI <- bind_cols(count_eir_CI, RR_eir_CI)
