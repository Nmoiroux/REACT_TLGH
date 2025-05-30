---
title: "Readme"
output: html_document
date: "2025-05-30"
---

##  Data and Analysis Repository

This repository contains the data and analyses scripts for a Randomized Controlled Trial evaluating Indoor Residual Spraying (IRS) or Behavioral Change Communication in combination to Long-Lasting Insecticidal Nets (LLIN) in Burkina Faso (BF) and Côte d'Ivoire (CI).

Results of this RCT have been published in Moiroux et al. 2025, The Lancet Global Health.

---

## Repository Structure
├── README.md # This file

├── metadata.Rmd # Metadata and dataset descriptions (see below)

├── Data/ # Folder containing all .RData databases

├── R/ # Folder containing all R scripts used in the analyses

---

## Metadata Overview

All datasets are described in `metadata.Rmd`.

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

- All datasets are available as `R` Objects in `.RData` databases and stored in the `Data/` folder.
- The `metadata.Rmd` provides a dynamic and structured summary of all datasets.
- Please cite appropriately if using this data or code.

---

## Reproducibility

Each R script is designed to be run independently and includes code to load data from the corresponding `.RData` database. We recommend setting the working directory to the root of this repository and sourcing each script from the `R/` folder.

---



