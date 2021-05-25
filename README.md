#Causal Inference Methods for Vaccine Sieve Analysis with Effect Modification

# Author Contributions Checklist Form

## Data

### Abstract

The data included with this submission includes a simulated version RTSS vaccine efficacy trials. The data are similar in structure to the real data used in the analysis.

### Availability

Due to existing privacy policies, it is difficult to provide the data used for RTSS analyses. However, it is theoretically possible to gain access to the RTSS data by applying for it through a concept proposal. We encourage interested readers to contact the authors for more information.

### Description

We provide the example data RTSS in the package survtmle, which contains the following variables: age (aggWeeks), gender (sex), study site indicator (site1-site5), distance from the nearest outpatient/inpatient facility (distOutpatient/distInpatient), height-for-age z-score (heightForAgeZscore), weight-for-age z-score (weightForAgeZscore), weight-for-height z-score (weightForHeightZscore), head circumference (armCircumZscore), hemoglobin(hemog), rainy/ dry season at enrollment (startMonthCat)

## Code

### Abstract

All of the simulation for this paper were conducted in Rstudio Server Version 1.2.1335. We provide the R code that to reproduce the simulation results that are included in the paper.

### Description

## Instructions for use

### Reproducibility 
The proposed methods require the following R software packages:
1, parallel version 3.5.3
2, survtmle 

survtmle package is included in the repository

### Notes

If you have questions regarding implementation of the method and codes, contact the authors.