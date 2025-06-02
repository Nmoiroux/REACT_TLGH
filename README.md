
---
##  Data and Analysis Repository

This repository contains the data and analyses scripts for a Randomized Controlled Trial evaluating Indoor Residual Spraying (IRS) or Behavioral Change Communication in combination to Long-Lasting Insecticidal Nets (LLIN) in Burkina Faso (BF) and Côte d'Ivoire (CI).

Results of this RCT have been published in: Moiroux et al. Non-pyrethroid indoor residual spraying or intensive behavior change communication in combination with long-lasting insecticidal nets against malaria disease in West Africa: a pragmatic transnational cluster-randomized controlled trial.
2025, The Lancet Global Health.

Suggested citation: 
- Moiroux, Nicolas; Taconet, Paul; Soma, Diloma Dieudonné; Zogo, Barnabas, 2025, "Replication Data and Codes for: Non-pyrethroid indoor residual spraying or intensive behavior change communication in combination with long-lasting insecticidal nets against malaria disease in West Africa: a pragmatic transnational cluster-randomized controlled trial.", https://doi.org/10.23708/MZ7KZZ, DataSuds, V1, UNF:6:FQq6r4pEK3i3eu1HnLHjXw== [fileUNF] 

---

## Repository Structure
|- README.md # This file

|- metadata.Rmd # R Markdown file of metadata and dataset descriptions

|- Data/ # Folder containing all .RData databases

|- R/ # Folder containing all R scripts used in the analyses

---

## Metadata Overview

All datasets are described in `metadata.Rmd`. The `metadata.Rmd` provides a structured summary of all datasets.

---

## Reproducibility

- All datasets are available as `R` Objects in `.RData` databases and stored in the `Data/` folder.
- Each R script is designed to be run independently and includes code to load data from the corresponding `.RData` database. We recommend setting the working directory to the root of this repository and sourcing each script from the `R/` folder.
- These scripts have been tested using `R` version 4.4.2 and required additional packages `tidyverse` 2.0.0, `metafor` 4.8-0, `glmmTMB` 1.1.11, `emmeans` 1.11.1, `boot` 1.3-31 and `rstatix` 0.7.2 (References below).

---

## R Scripts and Required Datasets

| R Script File                                         | Analysis                             | Required Datasets                                    | .Rdata database         |
|-------------------------------------------------------|--------------------------------------|------------------------------------------------------|-------------------------|
| `Meta_analysis_nonPYR_IRS_update.R`                   | Non-PYR IRS Meta-analysis            | `data_meta_IRS`                                      | data_meta_IRS.RData     |
| `Primary outcome_Incidence rate_Analysis.R`           | Incidence rates (Passive detection)  | `pas_ana_rate_subgroup`                              | data.RData              |
| `Secondary outcome_Anopheles EIR_Analysis.R`          | Entomolgical Incolutation Rate       | `eir_BF`, `eir_CI`                                   | entomo.RData            |
| `Secondary outcome_Anopheles Exophagy_Analysis.R`     | Exophagy                             | `ER`                                                 | entomo.RData            |
| `Secondary outcome_Anopheles HBR_Analysis.R`          | Human Biting Rate (Anopheles)        | `hbr`                                                | entomo.RData            |
| `Secondary outcome_Anopheles Hours_Analysis.R`        | Biting times (Anopheles)             | `Hour_BF`, `Hour_CI`                                 | entomo.RData            |
| `Secondary outcome_Anopheles PR_Analysis.R`           | Parity Rate (Anopheles)              | `PR_BF`, `PR_CI`                                     | entomo.RData            |
| `Secondary outcome_Anopheles Resistances_Analysis.R`  | kdr-w, kdr-e & ace-1 alleles freq.   | `KDRW_BF`, `KDRW_CI`, `KDRE_BF`, `ACE1_BF`, `ACE1_CI`| entomo.RData            |
| `Secondary outcome_Anopheles SR_Analysis.R`           | Sporozoite Rate (Anopheles)          | `SR_BF`, `SR_CI`                                     | entomo.RData            |
| `Secondary outcome_Case prevalence_Analysis.R`        | Case prevalence (Survey)             | `df_case`                                            | data.RData              |
| `Secondary outcome_Infection Prevalence_Analysis.R`   | Infection prevalence (Survey)        | `df_GE_BF`, `df_GE_CI`                               | data.RData              |
| `Secondary outcome_LLIN use_Analysis.R`               | LLIN use rate                        | `df_use`                                             | data2.RData             |
| `Secondary outcome_Mosquito HBR_Analysis.R`           | Human Biting Rate (all mosquitoes)   | `nuis`                                               | data.RData              |
| `Secondary outcome_Parasite density_Analysis.R`       | Parasite density (Survey)            | `df_dp_BF`, `df_dp_CI`                               | data.RData              |

---

## Notes

- Please cite appropriately if using this data or code.

---

## References

- R Core Team (2024). _R: A Language and Environment for Statistical Computing_. R
  Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.
- Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A,
  Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson
  D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K, Yutani H (2019).
  “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686.
  doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.
- Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal
  of Statistical Software, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03
- Mollie E. Brooks, Kasper Kristensen, Koen J. van Benthem, Arni Magnusson, Casper W.
  Berg, Anders Nielsen, Hans J. Skaug, Martin Maechler and Benjamin M. Bolker (2017).
  glmmTMB Balances Speed and Flexibility Among Packages for Zero-inflated Generalized
  Linear Mixed Modeling. The R Journal, 9(2), 378-400. doi: 10.32614/RJ-2017-066.
- Lenth R (2025). _emmeans: Estimated Marginal Means, aka Least-Squares Means_. R package
  version 1.11.1, <https://CRAN.R-project.org/package=emmeans>.
- Angelo Canty and Brian Ripley (2024). boot: Bootstrap R (S-Plus) Functions. R package
  version 1.3-31.
- Kassambara A (2023). _rstatix: Pipe-Friendly Framework for Basic Statistical Tests_. R
  package version 0.7.2, <https://CRAN.R-project.org/package=rstatix>.
  
  
  



