# secondary outcomes analysis (entomological outcomes)
## All mosquitoes HBR (nuisance)
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

### Analysis of nuisance ----
#### model
glmm_nuis_adj <- glmmTMB(nuis ~ prepost + prepost:intervention + codepays + (1|miss_area) + (1|codevillage/point/poste), data = nuis, family = nbinom2(link = "log"))

#### comparisons
RR_nuis <- emmeans(glmm_nuis_adj , ~intervention, type="response") %>%
	contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

#### count data 
count_nuis <- nuis %>%
	group_by(intervention, prepost) %>%
	summarise(n=sum(nuis), d= n(), m = mean(nuis), ci_low = ci(nuis)[4], ci_hig = ci(nuis)[5]) %>%
	mutate(mean = paste0(round(m,2),"[",round(ci_low,2),";",round(ci_hig,2), "] (",n,"/",d,")")) %>%
	select(-c(m,ci_low,ci_hig,n,d)) %>%
	pivot_wider(names_from = prepost, values_from = mean) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### results (Table 4)
res_nuis <- bind_cols(count_nuis, RR_nuis)


## interaction testing (country) ----

#### model
glmm_nuis_country <- glmmTMB(nuis ~ prepost + prepost:intervention*codepays + (1|miss_area) + (1|codevillage/point/poste), data = nuis, family = nbinom2(link = "log"))

#### comparisons
RRR_nuis <- emmeans::emmeans(glmm_nuis_country, ~intervention|codepays, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	assign(x="RR", value = .,envir = .GlobalEnv) %>%
	emmeans::contrast(list(`BF / CI BCC` =  c(1,0,-1,0), 
												 `BF / CI IRS` = c(0,1,0,-1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(RRR = paste0(round(ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,RRR,p.value) %>%
	add_row(RRR="1", .before = 2) %>%
	add_row(RRR="1", .before = 1)

#### table of results of the interaction analysis (Supplementary Table 19)
RR %>% as.data.frame() %>% 
	mutate(RR = paste0(round(ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(codepays, contrast,RR,p.value)	%>%
	arrange(contrast, desc(codepays)) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(RRR_nuis) %>%
	select(-contrast)
