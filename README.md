**Radboud University Nijmegen**  
Program: Business Analysis and Modeling  
Author: Ashkan Allahyari
Master Thesis — Integrating System Dynamics and Machine Learning to Model Social Determinants of Cognitive Decline: Evidence from the SHARE Dataset
---

## Project Overview

This repository contains the analytical codebase developed for my master’s thesis at Radboud University. The study uses longitudinal data from the Survey of Health, Ageing and Retirement in Europe (SHARE) to examine patterns in cognitive health, social relationships, and demographic behavior among older adults.

Due to the sensitive nature of the data and its access policy, **raw data is not included in this repository**.

---

## Data Access & Usage

The data used in this study is provided by the SHARE Research Infrastructure.

To obtain access to the SHARE dataset:
Visit the official SHARE data portal:  
https://share-eric.eu/

Once you receive approval and access credentials, download the required files and place them in the following directory:
\data\rawdata


The expected filenames follow the structure shown below, where `XXX` refers to the wave number (e.g., 4, 5, 6, etc.):

- `sharewX_rel9-0-0_gv_allwaves_cv_r`
- `sharewXXX_rel9-0-0_dn`
- `sharewXXX_rel9-0-0_gv_networks`
- `sharewXXX_rel9-0-0_ac.sav`
- `sharewXXX_rel9-0-0_sn.sav`
- `sharewXXX_rel9-0-0_ch`
- `sharewXXX_rel9-0-0_sp`
- `sharewXXX_rel9-0-0_mh`
- `sharewXXX_rel9-0-0_cf`
- `sharewXXX_rel9-0-0_ph`

---

## Disclaimer

This repository does **not** contain any SHARE raw data files due to access restrictions. You must register with SHARE and obtain approval to download and use the data for academic purposes.  
The author bears no responsibility for unauthorized data sharing or access violations.

---

## Keywords

SHARE, longitudinal data, cognitive health, social networks, aging, Europe, Radboud University, business analysis, R 