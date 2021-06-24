# Causal Inference Methods for Vaccine Sieve Analysis with Effect Modification

## Author Contributions Checklist Form

### Data

#### Abstract

The data included with this submission includes a simulated version RTSS vaccine efficacy trials. The data are similar in structure to the real data used in the analysis.

#### Availability

Due to existing privacy policies, the real data used for the RTSS analyses
cannot be shared; however, we have made available a modified version of the true
data set that can be used to run our code, as described below.

#### Description

We provide the example data RTSS in the `rtssAnalysis/Data` folder in the form
of an `.RData` object. The object can be loaded in `R` using the `load`
function. This places a 10-length `list` named `dataList`. Each entry in the
`list` contains a `data.frame` with the following variables: 

* `ftime` = failure time (in months)
* `ftype` = the failure type indicator (=0 if no event, 1 + Hamming distance
otherwise)
* `ageWeeks`, `sex`, `site1`-`site5` (study site indicators),
`distOutpatient`/`distInpatient` (distance from the nearest outpatient/inpatient
facility), `heightForAgeZscore`, `weightForAgeZscore`, `weightForHeightZscore`,
`armCircumZscore`, `hemog` (hemoglobin), `startMonthCat` (indicator of rainy/
dry season at enrollment)

Though in the real data analysis, we used 1000 outputed data sets, the example
data contain only 10 data sets, for computational simplicity.

### Code

#### Abstract

All of the simulations for this paper were conducted in `R` version 4.0.4. We
provide the R code that to reproduce the simulation results that are included in
the paper, as well as code that was used in our data analysis, which can be used
to analyze a mock data set.

### Description

### Use and reproducibility 

This repository uses [`renv`]
(https://rstudio.github.io/renv/articles/renv.html) for package management.
To set the environment, you can execute the following code at the command line.

```bash
# from the repository root directory
Rscript -e "install.packages('renv'); renv::restore()"
```

This will install all R packages used in the simulations and data analysis.

The main functions for computing estimates and confidence intervals for MSM
parameters are contained in the `survtmle` package, which is loaded by each
simulation using `devtools::load_all`.

To run the simulations, execute the following at the command line.

```bash
make simulationOne
make simulationTwo
```

The simulations are run on a `SOCK` cluster using the `parallel` package.
Result are subsequently compiled into an `html` document that can be found in
the respective subfolders.

To run the mock RTSS data analysis, execute the following at the command line.

```bash
make rtssAnalysis
```

The analysis of each multiple outputed data sets are run on a `SOCK` cluster
and a summary table is shown in an `html` document in the `rtssAnalysis` folder.


### Notes

If you have questions regarding implementation of the method and codes, contact 
[David Benkeser](mailto:benkeser@emory.edu).