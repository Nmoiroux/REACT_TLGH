# primary outcome analysis
## incidence rate measured by Passive Case Detection
### Load required packages
library(tidyverse)
library(glmmTMB)
library(emmeans)

### Load data
load("Data/data.RData")

### summary data ----
#### pop under follow-up
pop_followup_pas <- pas_ana_rate_subgroup %>%
	group_by(codepays,Interv,prepost,codevillage,sum) %>%
	summarise(n=n()) %>%
	ungroup %>%
	group_by(Interv,prepost) %>%
	summarise(pop=sum(sum))

#### summary stats
count_pas <- pas_ana_rate_subgroup %>%	group_by(prepost,Interv) %>%
	summarise(sn = sum(n), n = n_distinct(week_n)) %>%
	left_join(pop_followup_pas, by = c("Interv","prepost")) %>%
	mutate(fol = pop*n/4.3, inc = sn/fol*100) %>%
	mutate(inc_t = paste0(round(inc,2)," (",sn,"/",round(fol),")")) %>%
	select(-c(sn,n,pop,fol,inc)) %>%
	pivot_wider(names_from = prepost, values_from = inc_t) %>%
	rename(preintervention = `0`, postintervention = `1`)

### Analysis ----
#### unadjusted model
glm_pas <- glmmTMB(n~prepost + prepost:Interv + offset(logpop) + ar1(week_f-1|codepays) + (1|csps_csu/codevillage),
									 data = pas_ana_rate_subgroup,
									 family=nbinom2(link = "log"))

#### adjusted model
glm_pas_adj <- glmmTMB(n~prepost + prepost:Interv +  age_c  + codepays +  offset(logpop) +
											 	ar1(week_f-1|codepays) + (1|csps_csu/codevillage),
											 data = pas_ana_rate_subgroup,
											 family=nbinom2(link = "log"))

#### Interventions effect 
adjust_meth <- "none" # specify the adjustment (CI and p-value) adjustment method

RR_pas <- emmeans::emmeans(glm_pas, ~Interv, type="response", data=pas_ana_rate_subgroup, offset = log(80)) %>%
	assign(x="EMM", value = ., envir = .GlobalEnv) %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

RR_pas_adj <- emmeans::emmeans(glm_pas_adj, ~Interv, type="response", data=pas_ana_rate_subgroup, offset = log(80)) %>%
	assign(x="EMM", value = ., envir = .GlobalEnv) %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

### Results table (primary outcome analysis - Table 2)
res_pas <- bind_cols(count_pas, RR_pas, RR_pas_adj)


### Interaction analysis ----
#### model (include interactions)
glm_pas_subgroup <- glmmTMB(n~prepost + prepost:Interv * age_c * codepays  + offset(logpop)  + ar1(week_f-1|codepays) + (1|csps_csu/codevillage),
														data = pas_ana_rate_subgroup,
														family=nbinom2(link = "log"))

#### test of interaction (age effect): is the effect of intervention (IEC or IRS) different btw age classes (0-5 vs 5-18) ?
RRR_pas_age <- emmeans::emmeans(glm_pas_subgroup, ~Interv|age_c, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	assign(x="RR_a", value = ., envir = .GlobalEnv) %>%
	pairs(simple="age_c", reverse =T, infer=TRUE, adjust = adjust_meth) %>%
	# emmeans::contrast(list(`5-18 / 0-5 BCC` =  c(-1,0,1,0,0,0),
	# 										 `>18 / 0-5 BCC` =  c(-1,0,0,0,1,0),
	# 										 `5-18 / 0-5 IRS` = c(0,-1,0,1,0,0),
	# 										 `>18 / 0-5 IRS` = c(0,-1,0,0,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>% slice(-c(3,6)) %>%
	mutate(RRR = paste0(round(ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,RRR,p.value)%>%
	add_row(RRR="1", .before = 3) %>%
	add_row(RRR="1", .before = 1)

#### table of results of the interaction analysis (age group - Supplementary Table 2)
RR_a %>% as.data.frame() %>% 
	mutate(RR = paste0(round(ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(age_c, contrast,RR,p.value)	%>%
	arrange(contrast) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(RRR_pas_age) %>%
	select(-contrast)

#### test of interaction (country effect): is the effect of intervention (IEC or IRS) different btw areas (BF vs CI) ?
RRR_pas_country <- emmeans::emmeans(glm_pas_subgroup, ~Interv|codepays, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	assign(x="RR_c", value = ., envir = .GlobalEnv) %>%
	pairs(simple="codepays", reverse =T, infer=TRUE, adjust = adjust_meth) %>%
	# emmeans::contrast(list(`CI / BF BCC` =  c(-1,0,1,0), 
	# 											 `CI / BF IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(RRR = paste0(round(ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,RRR,p.value) %>%
	add_row(RRR="1", .before = 2) %>%
	add_row(RRR="1", .before = 1)

#### table of results of the interaction analysis (area - Supplementary Table 3)
RR_c %>% as.data.frame() %>% 
	mutate(RR = paste0(round(ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(codepays, contrast,RR,p.value)	%>%
	arrange(contrast) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(RRR_pas_country) %>%
	select(-contrast)


### Analysis of primary outcome per country (subgroup analysis) ----
area <- "BF" # Choose "CI" or "BF"

pas_ana_rate_subgroup_c <- pas_ana_rate_subgroup %>%
	filter(codepays == area)

#### pop under follow-up
pop_followup_pas <- pas_ana_rate_subgroup_c %>%
	group_by(codepays,Interv,prepost,codevillage,sum) %>%
	summarise(n=n()) %>%
	ungroup %>%
	group_by(Interv,prepost) %>%
	summarise(pop=sum(sum))

#### summary stats
count_pas <- pas_ana_rate_subgroup_c %>%	group_by(prepost,Interv) %>%
	summarise(sn = sum(n), n = n_distinct(week_n)) %>%
	left_join(pop_followup_pas, by = c("Interv","prepost")) %>%
	mutate(fol = pop*n/4.3, inc = sn/fol*100) %>%
	mutate(inc_t = paste0(round(inc,2)," (",sn,"/",round(fol),")")) %>%
	select(-c(sn,n,pop,fol,inc)) %>%
	pivot_wider(names_from = prepost, values_from = inc_t) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### unadjusted model
glm_pas <- glmmTMB(n~prepost + prepost:Interv + offset(logpop) + ar1(week_f-1|1) + (1|csps_csu/codevillage),
									 data = pas_ana_rate_subgroup_c,
									 family=nbinom2(link = "log"))

#### adjusted model
glm_pas_adj <- glmmTMB(n~prepost + prepost:Interv + offset(logpop) + age_c +
											 	ar1(week_f-1|1) + (1|csps_csu/codevillage),
											 data = pas_ana_rate_subgroup_c,
											 family=nbinom2(link = "log"))

#### comparisons
adjust_meth <- "none" # 

RR_pas <- emmeans::emmeans(glm_pas, ~Interv, type="response", data=pas_ana_rate_subgroup, offset = log(80)) %>%
	assign(x="EMM", value = ., envir = .GlobalEnv) %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

RR_pas_adj <- emmeans::emmeans(glm_pas_adj, ~Interv, type="response", data=pas_ana_rate_subgroup, offset = log(1)) %>%
	assign(x="EMM", value = ., envir = .GlobalEnv) %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE, adjust = adjust_meth) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

#### results of subgroup (area) analysis (Supplementary Table 4)
res_pas <- bind_cols(count_pas, RR_pas, RR_pas_adj)
res_pas
