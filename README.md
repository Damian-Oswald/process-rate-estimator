> [!NOTE]  
> This repo is still very much work in progress.

Process Rate Estimator
======================
Damian Oswald
September 23, 2023

# How can I run the Process Rate Estimator?

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
[^1]: This R package contains all functions that are repeatedly used throughout the workflow of this project. It's stored [on its own GitHub repository](https://github.com/Damian-Oswald/PRE/).
