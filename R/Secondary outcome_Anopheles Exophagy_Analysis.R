# secondary outcomes analysis (entomological outcomes)
## Anopheles Exophagy Rate
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

### Exphagy rate analysis ----
#### count data
count_ER <- ER %>%
	group_by(intervention, prepost) %>%
	summarise(n= sum(er, na.rm=TRUE),N=n(), m = mean(er,na.rm=TRUE), ci_low = ci(er)[4], ci_hig = ci(er)[5]) %>%
	mutate(mean = paste0(round(m,4)*100,"% [",round(ci_low,4)*100,";",round(ci_hig,4)*100, "] (",n,"/",N,")")) %>%
	select(-c(m,ci_low,ci_hig,n,N)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### model unadjusted
glm_ER <- glmmTMB(er ~prepost + prepost:intervention + (1|nummission) + (1|codevillage/point), data = ER, family = binomial(link = "logit"))

#### model adjusted (for area)
glm_ER_adj <- glmmTMB(er ~prepost + prepost:intervention + codepays + (1|nummission) + (1|codevillage/point), data = ER, family = binomial(link = "logit"))

#### comparisons
OR_ER <- emmeans(glm_ER , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

OR_ER_adj <- emmeans(glm_ER_adj , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### results (Supplementary Table 23)
res_ER <- bind_cols(count_ER, OR_ER, OR_ER_adj)


### interaction testing (country) ----

#### model
glmm_ER_country <- glmmTMB(er ~prepost + prepost:intervention * codepays + (1|nummission) + (1|codevillage/point), data = ER, family = binomial(link = "logit"))

#### comparisons
RRR_ER <- emmeans::emmeans(glmm_ER_country, ~intervention|codepays, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	assign(x="OR", value = .,envir = .GlobalEnv) %>%
	emmeans::contrast(list(`BF / CI BCC` =  c(1,0,-1,0), 
												 `BF / CI IRS` = c(0,1,0,-1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis (Supplementary Table 24)
OR %>% as.data.frame() %>% 
	mutate(RR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]"))%>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(codepays, contrast,RR,p.value)	%>%
	arrange(contrast, desc(codepays)) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(RRR_ER) %>%
	select(-contrast)
