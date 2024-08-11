# :test_tube: Process Rate Estimator

This repository contains the source code and documentation for the process rate estimator. For a detailed (scientific) description of the process rate estimator, see Decock et al. ([2022](https://doi.org/10.5194/bg-2022-221)).

The project was done for the [sustainable agro-ecosystems group at ETH Zurich](https://sae.ethz.ch/). While the project documentation is presented on [this website](https://damian-oswald.github.io/process-rate-estimator/), the source code for the process rate estimator in contained in an R package called `PRE`. You can explore said package in [this GitHub repository](https://github.com/damian-oswald/PRE/). Meanwhile, this repository contains the scripts for running the process rate estimator on some collected data as well as the source code for reproducing the documentation page.

# :question: How can I run the process rate estimator?

To run the process rate estimator, first install the R package `PRE`.[^readme-1]

[^readme-1]: This R package contains all functions that are repeatedly used throughout the workflow of this project. It's stored [on its own GitHub repository](https://github.com/Damian-Oswald/PRE/).

``` r
remotes::install_github("https://github.com/Damian-Oswald/PRE")
```

After successful installation, you can clone this GitHub repository.

``` bash
git clone https://github.com/Damian-Oswald/process-rate-estimator
cd process-rate-estimator
```

## :globe_with_meridians: Reproducing the documentation site

The [documentation site](https://damian-oswald.github.io/process-rate-estimator/) was built using the [scientific publishing system Quarto](https://quarto.org). To reproduce the documentation, first install Quarto, then run `quarto render` to render the entire site.

## :microbe: Running the main script

After successful installation of the R package `PRE` and cloning of this repository, you can run the main script, which will reproduce the results for the process rate estimations.

``` bash
Rscript scripts/run-process-rate-estimator/run-PRE.R
```

These results are also documented [here](https://damian-oswald.github.io/process-rate-estimator/results.html).

## :abacus: Running the sensitivity analysis

To reproduce the sensitivity analysis (which is documented [here]()https://damian-oswald.github.io/process-rate-estimator/sensitivity-analysis.html), enter the following command:

``` bash
Rscript scripts/sensitivity-analysis/sensitivitiy-analysis.R
```

> [!IMPORTANT]  
> Running the sensitivity analysis R script is very compute intensive and may take a few days on an ordinary computer.

You can open the corresponding R script to change parameters such as `SAMPLESIZE` and `SAMPLEREPEAT`; however, doing this will change the exact numerical results.[^readme-2]

[^readme-2]: Although the *expected* results will not change.

The results written by the sensitivity analysis are also used by the uncertainty analysis.