:test_tube: Process Rate Estimator
==================================

# How can I run the process rate estimator?

To run the process rate estimator, first install the R package `PRE`.[^1]

```r
remotes::install_github("https://github.com/Damian-Oswald/PRE")
```

After successful installation, you can clone this GitHub repository.

```bash
git clone https://github.com/Damian-Oswald/process-rate-estimator
cd process-rate-estimator
```

With this, you are ready to run the main script, which will reproduce the results for the process rate estimations.

```bash
Rscript scripts/run-process-rate-estimator/run-PRE.R
```

The scripts to reproduce the study results are divided by topic.

## :abacus: Sensitivity analysis

To reproduce the sensitivity analysis, enter the following command:

```bash
Rscript scripts/sensitivity-analysis/sensitivitiy-analysis.R
```

Note that you can open the corresponding file to change parameters such as `SAMPLESIZE` and `SAMPLEREPEAT`; however, doing this will change the exact numerical results.[^2]

[^1]: This R package contains all functions that are repeatedly used throughout the workflow of this project. It's stored [on its own GitHub repository](https://github.com/Damian-Oswald/PRE/).

[^2]: Although the *expected* results will not change.
