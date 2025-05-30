# secondary outcomes analysis (other outcomes)
## LLIN use measured by Cross-sectional surveys (CSS)
### Load required packages
library(tidyverse)
library(glmmTMB)
library(emmeans)

### Load data
load("Data/data2.RData")


### LLIN use analysis
#### LLIN use statistics (pre- and post-intervention per arms in both country)
var <- "use"
df_use %>%
	mutate(var = get(var)) %>%
	group_by(codepays,prepost,Interv,var) %>%
	summarise(n=n()) %>%
	filter(!is.na(var)) %>%
	pivot_wider(names_from = var, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p) %>%
	rename(preintervention = `0`, postintervention = `1`)

#### LLIN use statistics (pre- and post-intervention per arms) (Table 1)
count_use <- df_use %>%
	group_by(prepost,Interv,use) %>%
	summarise(n=n()) %>%
	filter(!is.na(use)) %>%
	pivot_wider(names_from = use, values_from = n) %>%
	mutate(N=`0`+`1`) %>%
	select(-c(`0`)) %>%
	ungroup() %>%
	mutate(pct = `1`/N) %>%
	mutate(pct = round(pct*100,2)) %>%
	mutate(p_use = paste0(pct, "% (",`1`,"/",N,")")) %>%
	select(-c(`1`,N,pct)) %>%
	pivot_wider(names_from = prepost, values_from = p_use) %>%
	rename(preintervention = `0`, postintervention = `1`)



#### cLDA analysis
glm_css_use <- glmmTMB::glmmTMB(use ~ prepost + prepost:Interv + age_c + tmin + codepays + (1|enq_loc) + (1|codevillage/codeindividu), 
																family=binomial(link = "logit"), 
																data = df_use)

#### OR use
OR_use <- emmeans::emmeans(glm_css_use , ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame()	%>%
	mutate(OR = paste0(round(odds.ratio,2),"[",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = FALSE)) %>%
	add_row(OR="1", .before = 1) %>%
	select(OR,p.value)

#### LLIN use results table (Supplementary Table 15)
bind_cols(count_use, OR_use)

### Interaction Analysis
#### model includes interactions
glm_use_subgroups <- glmmTMB::glmmTMB(use ~ prepost + prepost:Interv*age_c*codepays + tmin +(1|enq_loc) + (1|codevillage/codeindividu), 
																			family=binomial(link = "logit"), 
																			data = df_use)

#### test of interaction (age effect): is the effect of intervention (IEC or IRS) different btw age classes (0-5 vs 5-18) ?
ORR_use_age <- emmeans::emmeans(glm_use_subgroups, ~Interv|age_c, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	assign(x="OR", value = .,envir = .GlobalEnv) %>%
	emmeans::contrast(list(`5-18 / 0-5 BCC` =  c(-1,0,1,0), 
												 `5-18 / 0-5 IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis (age classes - Supplementary Table 16)
OR %>% as.data.frame() %>% 
	mutate(OR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(age_c, contrast,OR,p.value)	%>%
	arrange(contrast, age_c) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(ORR_use_age) %>%
	select(-contrast)

#### test of interaction (area effect): is the effect of intervention (IEC or IRS) different btw areas (BF vs CIV) ?
ORR_use_country <- emmeans::emmeans(glm_use_subgroups, ~Interv|codepays, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	assign(x="OR", value = .,envir = .GlobalEnv) %>%
	pairs(simple="codepays", reverse =T, infer=TRUE) %>%
	#emmeans::contrast(list(`BF / CI BCC` =  c(-1,0,1,0), 
	#											 `BF / CI IRS` = c(0,-1,0,1)), infer = TRUE, by=NULL) %>%
	as.data.frame() %>%
	mutate(ORR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(contrast,ORR,p.value) %>%
	add_row(ORR="1", .before = 2) %>%
	add_row(ORR="1", .before = 1)

#### table of results of the interaction analysis (area - Supplementary Table 17)
OR %>% as.data.frame() %>% 
	mutate(OR = paste0(round(odds.ratio,2)," [",round(asymp.LCL,2),";",round(asymp.UCL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	select(codepays, contrast,OR,p.value)	%>%
	arrange(contrast, codepays) %>%
	mutate(contrast = substr(contrast,1,3)) %>%
	rename(arm = contrast) %>%
	select(-p.value) %>%
	bind_cols(ORR_use_country) %>%
	select(-contrast)


