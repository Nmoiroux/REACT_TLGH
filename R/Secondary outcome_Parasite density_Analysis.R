# secondary outcomes analysis (epidemiological outcomes)
## Parasite density in asymptomatics measured by Cross-sectional surveys (CSS)
### Load required packages
library(tidyverse)
library(glmmTMB)
library(emmeans)

### Load data
load("Data/data.RData")

### model BF
#### parasite densities (asympto) stats
count_dp_BF <- df_dp_BF %>%
	mutate(lg_dp = log(dp_pf)) %>%
	group_by(prepost, Interv) %>%
	summarise(m=mean(lg_dp, na.rm = TRUE), sd=sd(lg_dp,na.rm=TRUE), n=n()) %>%
	ungroup() %>%
	mutate(gm = exp(m), low=exp(m-1.96*sd/(n^0.5)), hig=exp(m+1.96*sd/(n^0.5))) %>% # calculate geometric mean and 95%CI
	mutate(dp = paste0(round(gm),"[",round(low),";",round(hig),"]")) %>%
	select(-c(m,sd,gm,low,hig)) %>%
	pivot_wider(names_from = prepost, values_from = c(n,dp)) %>%
	relocate(dp_0, .before = n_1) %>%
	mutate(pre = paste0(dp_0," (",n_0,")"),post = paste0(dp_1," (",n_1,")")) %>%
	select(Interv, pre, post)


#### model
glmm_dp_BF <- glmmTMB::glmmTMB(log(dp_pf) ~ prepost + prepost:Interv+age+(1|enq_loc)+(1|codevillage/codeindividu), family=gaussian(link = "identity"), data = df_dp_BF)

#### comparisons between arm pre- and post-intervention
OR_dp_BF <-  emmeans::emmeans(glmm_dp_BF, ~Interv, type="response") %>%
	emmeans::contrast("trt.vs.ctrl", ref="Ctrle prepost1", exclude = c("IEC prepost0","IRS prepost0","Ctrle prepost0"), infer=TRUE) %>%
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(lower.CL,2),";",round(upper.CL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

#### result table (Table 3)
mod_res_dp_BF <- bind_cols(count_dp_BF, OR_dp_BF)
mod_res_dp_BF


### model CI
#### parasite densities (asympto) stats
count_dp_CI <- df_dp_CI %>%
	mutate(lg_dp = log(dp_pf)) %>%
	group_by(Interv) %>%
	summarise(m=mean(lg_dp, na.rm = TRUE), sd=sd(lg_dp,na.rm=TRUE), n=n()) %>%
	ungroup() %>%
	mutate(gm = exp(m), low=exp(m-1.96*sd/(n^0.5)), hig=exp(m+1.96*sd/(n^0.5)))%>%
	mutate(dp = paste0(round(gm),"[",round(low),";",round(hig),"]")) %>%
	select(-c(m,sd,gm,low,hig)) %>%
	mutate(post = paste0(dp," (",n,")")) %>%
	select(Interv, post)

#### model
glmm_dp_CI <- glmmTMB::glmmTMB(log(dp_pf) ~ Interv+age+sexe+(1|enq_loc)+(1|codevillage/codeindividu), family=gaussian(link = "identity"), data = df_dp_CI)

#### comparisons between arm post-intervention
OR_dp_CI <- summary(emmeans::emmeans(glmm_dp_CI, trt.vs.ctrl1~Interv), infer=TRUE, type="response")$contrasts %>% 
	as.data.frame() %>%
	mutate(RR = paste0(round(ratio,2),"[",round(lower.CL,2),";",round(upper.CL,2),"]")) %>%
	mutate(p.value = format(p.value, digits = 3, scientific = TRUE)) %>%
	add_row(RR="1", .before = 1) %>%
	select(RR,p.value)

#### result table
mod_res_dp_CI <- bind_cols(count_dp_CI, OR_dp_CI)
mod_res_dp_CI
