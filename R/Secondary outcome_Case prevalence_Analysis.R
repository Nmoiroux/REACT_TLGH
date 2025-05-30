# secondary outcomes analysis (epidemiological outcomes)
## case prevalence measured by Cross-sectional surveys (CSS)
### Load required packages
library(tidyverse)
library(glmmTMB)
library(emmeans)

### Load data
load("Data/data.RData")


#### Statistics on row data
#### count of cases and observations
count <- df_case %>% group_by(prepost,Interv,case) %>%
	summarise(n=n()) %>%
	filter(!is.na(case)) %>%
	pivot_wider(names_from = case, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_inc = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p_inc) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### model unadjusted
glm_case <- glmmTMB::glmmTMB(case ~ prepost + prepost:Interv + (1|enq_loc) + (1|codevillage/codeindividu), 
														 family=binomial(link = "logit"), data = df_case)

#### model adjusted (age and country)
glm_case_adj <- glmmTMB::glmmTMB(case ~ prepost + prepost:Interv + age  + codepays + (1|enq_loc) + (1|codevillage/codeindividu), 
																 #dispformula = ~0,
																 family=binomial(link = "logit"), data = df_case)

#### comparisons between arm
adjust_meth <- "none"
OR <- emmeans::emmeans(glm_case, ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = FALSE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

OR_adj <- emmeans::emmeans(glm_case_adj, ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = FALSE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### result table (Table 2)
bind_cols(count, OR, OR_adj)


### Interaction analysis ----
#### model (include interactions)
glm_case_subgroups <- glmmTMB::glmmTMB(case ~ prepost + prepost:Interv * age_c * codepays + (1|enq_loc) + (1|codevillage/codeindividu), 
																			 family=binomial(link = "logit"), data = df_case)

#### test of interaction (age effect): is the effect of intervention (IEC or IRS) different btw age classes (0-5 vs 5-18) ?
ORR_case_age <- emmeans::emmeans(glm_case_subgroups, ~Interv|age_c, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	assign(x="OR_a", value = .,envir = .GlobalEnv) %>%
	pairs(simple="age_c", reverse =T, infer=TRUE, adjust = adjust_meth) %>%
	# emmeans::contrast(list(`5-18 / 0-5 BCC` =  c(-1,0,1,0), 
	#											 `5-18 / 0-5 IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis (Supplementary Table 6)
OR_a %>% as.data.frame() %>% 
	mutate(OR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(age_c, contrast,OR,p.value)	%>%
	arrange(contrast, age_c) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(ORR_case_age) %>%
	select(-contrast)


#### test of interaction (country effect): is the effect of intervention (IEC or IRS) different btw areas (BF vs CI) ?
ORR_case_country <- emmeans::emmeans(glm_case_subgroups, ~Interv|codepays, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	assign(x="OR_c", value = .,envir = .GlobalEnv) %>%
	pairs(simple="codepays", reverse =T, infer=TRUE, adjust = adjust_meth) %>%
	# emmeans::contrast(list(`CI / BF BCC` =  c(-1,0,1,0), 
	# 											 `CI / BF IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis  (Supplementary Table 7)
OR_c %>% as.data.frame() %>% 
	mutate(OR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(codepays, contrast,OR,p.value)	%>%
	arrange(contrast, codepays) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(ORR_case_country) %>%
	select(-contrast)

### Analysis of case prevalence per country (subgroup analysis)----
area <- "CI" # Choose "CI" or "BF"

df_case_c <- df_case %>%
	filter(codepays == area)

#### Statistics on row data
#### count of cases and observations
count <- df_case_c %>% group_by(prepost,Interv,case) %>%
	summarise(n=n()) %>%
	filter(!is.na(case)) %>%
	pivot_wider(names_from = case, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_inc = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p_inc) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### model unadjusted
glm_case <- glmmTMB::glmmTMB(case ~ prepost + prepost:Interv + (1|enq_loc) + (1|codevillage/codeindividu), 
														 family=binomial(link = "logit"), data = df_case_c)

#### model adjusted (age and country)
glm_case_adj <- glmmTMB::glmmTMB(case ~ prepost + prepost:Interv + age  + (1|enq_loc) + (1|codevillage/codeindividu), 
																 #dispformula = ~0,
																 family=binomial(link = "logit"), data = df_case_c)


#### comparisons between arm
adjust_meth <- "none"
OR <- emmeans::emmeans(glm_case, ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = FALSE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

OR_adj <- emmeans::emmeans(glm_case_adj, ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = FALSE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#  (Supplementary Table 8)
mod_res <- bind_cols(count, OR, OR_adj)
mod_res 

