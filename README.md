> [!NOTE]  
> This repo is still very much work in progress.

Process Rate Estimator
======================
Damian Oswald
September 23, 2023

# How can I run the Process Rate Estimator?

To run the process rate estimator, first install the `PRE` R package.

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
cd scripts/run-process-rate-estimator
Rscript run-PRE.R
```