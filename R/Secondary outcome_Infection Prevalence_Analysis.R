# secondary outcomes analysis (epidemiological outcomes)
## Prevalence of infection (blood-smears positivity) measured by Cross-sectional surveys (CSS)
### Load required packages
library(tidyverse)
library(glmmTMB)
library(emmeans)

### Load data
load("Data/data.RData")

### BF area analysis
#### summary statistics
count_GE_BF <- df_GE_BF %>% group_by(prepost,Interv, prev) %>%
	summarise(n=n()) %>%
	filter(!is.na(prev)) %>%
	pivot_wider(names_from = prev, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_inc = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p_inc) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### model adjusted
glm_GE_BF <- glmmTMB::glmmTMB(prev ~ prepost + prepost:Interv+age+(1|enq_loc)+(1|codevillage/codeindividu), family=binomial(link = "logit"), data = df_GE_BF)

#### comparisons between arms pre- and post-intervention
OR_GE_BF <- emmeans::emmeans(glm_GE_BF, ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### result table (Table 3)
mod_res_GE_BF <- bind_cols(count_GE_BF, OR_GE_BF)
mod_res_GE_BF


### CIV area analysis
#### summary statistics
count_GE_CI <- df_GE_CI %>% group_by(Interv, prev) %>%
	summarise(n=n()) %>%
	filter(!is.na(prev)) %>%
	pivot_wider(names_from = prev, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_inc = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	rename(postintervention = p_inc)

#### model
glm_GE_CI <- glmmTMB::glmmTMB(prev ~ Interv+age+(1|enq_loc)+(1|codevillage/codeindividu), family=binomial(link = "logit"), data = df_GE_CI)


#### comparisons between arm post-intervention
OR_GE_CI <- summary(emmeans::emmeans(glm_GE_CI, trt.vs.ctrl1~Interv), type="response", infer=TRUE)$contrasts %>% 
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### result table (Table 3)
mod_res_GE_CI <- bind_cols(count_GE_CI, OR_GE_CI)
mod_res_GE_CI

### Interaction analysis (effect of age groups) - BF area
#### stats for BF (Supplementary Table 13)
count_GE_BF_age <- df_GE_BF %>% 
	mutate(age_c = age %>% cut( breaks=c(-1, 5, 18, +Inf), 
															labels=c("0-5", "5-18", ">18")) %>% as.factor()) %>%
	group_by(prepost,Interv, age_c, prev) %>%
	summarise(n=n()) %>%
	filter(!is.na(prev)) %>%
	pivot_wider(names_from = prev, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_inc = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p_inc) %>%
	rename(preintervention = `0`, postintervention = `1`)


# model adjusted
glm_GE_BF_subgroup <- glmmTMB::glmmTMB(prev ~ prepost + prepost:Interv*age_c+(1|enq_loc)+(1|codevillage/codeindividu), family=binomial(link = "logit"), data = df_GE_BF)

# test of interaction (age effect): is the effect of intervention (IEC or IRS) different btw age classes (0-5 vs 5-18) ?
ORR_GE_BF_age <- emmeans::emmeans(glm_GE_BF_subgroup, ~Interv|age_c, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust="none" ) %>%
	assign(x="OR_BF", value = .,envir = .GlobalEnv) %>%
	emmeans::contrast(list(`5-18 / 0-5 BCC` =  c(-1,0,1,0), 
												 `5-18 / 0-5 IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis (Supplementary Table 14)
OR_BF %>% as.data.frame() %>% 
	mutate(OR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(age_c, contrast,OR,p.value)	%>%
	arrange(contrast, age_c) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(ORR_GE_BF_age) %>%
	select(-contrast)

### Interaction analysis (effect of age groups) - CIV area
#### stats for CIV (Supplementary Table 13)
count_GE_CI_age <- df_GE_CI %>% 
	mutate(age_c = age %>% cut( breaks=c(-1, 5, 18, +Inf), 
															labels=c("0-5", "5-18", ">18")) %>% as.factor()) %>%
	group_by(prepost,Interv, age_c, prev) %>%
	summarise(n=n()) %>%
	filter(!is.na(prev)) %>%
	pivot_wider(names_from = prev, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_inc = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p_inc) %>%
	rename(postintervention = `1`)

#### adjusted model
glm_GE_CI_subgroup <- glmmTMB::glmmTMB(prev ~ Interv*age_c+(1|enq_loc)+(1|codevillage/codeindividu), family=binomial(link = "logit"), data = df_GE_CI)

#### test of interaction (age effect): is the effect of intervention (IEC or IRS) different btw age classes (0-5 vs 5-18) ?
ORR_GE_CI_age <- emmeans::emmeans(glm_GE_CI_subgroup, ~Interv|age_c, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle", infer=TRUE, adjust="none" ) %>%
	assign(x="OR_CI", value = .,envir = .GlobalEnv) %>%
	emmeans::contrast(list(`5-18 / 0-5 BCC` =  c(-1,0,1,0), 
												 `5-18 / 0-5 IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis  (Supplementary Table 14)
OR_CI %>% as.data.frame() %>% 
	mutate(OR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(age_c, contrast,OR,p.value)	%>%
	arrange(contrast, age_c) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(ORR_GE_CI_age) %>%
	select(-contrast)
