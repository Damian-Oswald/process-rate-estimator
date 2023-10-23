# Process Rate Estimator
Damian Oswald
September 23, 2023

# Introduction

Denitrification is the natural process by which nitrate
(NO<sub>3</sub><sup>-</sup>) in the soil are converted by bacteria into
nitrous oxide (N<sub>2</sub>O) or pure nitrigen (N<sub>2</sub>). The
latter is called *total denitrification* — the full process described in
[Equation 1](#eq-denitrification) takes place.

<span id="eq-denitrification">$$
\ce{NO3^- ->[\text{Nitrate}][\text{reductase}] NO2^- ->[\text{Nitrite}][\text{reductase}] NO ->[\text{Nitrite oxide}][\text{reductase}] N2O^- ->[\text{Nitrous oxide}][\text{reductase}] N2}
 \qquad(1)$$</span>

Denitrification occurs in conditions where oxygen is limited, such as
waterlogged soils. It is part of the nitrogen cycle, where nitrogen is
circulated between the atmosphere, organisms and the earth.

# Formal model description

## Model parameters

<div id="tbl-parameters">

| Symbol              | Code          | Name                                                                                 | Value                         | Unit                        |
|:--------------------|:--------------|:-------------------------------------------------------------------------------------|:------------------------------|:----------------------------|
| $BD$                | `BD`          | Bulk density (mass of the many particles of the material divided by the bulk volume) | $1.686$                       | g cm<sup>-3</sup>           |
| $\theta_w$          | `theta_w`     | Soil volumetric water content                                                        |                               |                             |
| $\theta_a$          | `theta_a`     | Air-filled porosity                                                                  | $1-\frac{\theta_w}{\theta_t}$ |                             |
| $\theta_t$          | `theta_t`     | Total soil porosity                                                                  | $1-\frac{BD}{2.65}$           |                             |
| $\text T$           | `temperature` | Soil temperature                                                                     | $298$                         | K                           |
| $D_{\text{s}}$      | `D_s`         | Gas diffusion coefficient                                                            | [Equation 3](#eq-Ds)          | m<sup>2</sup>s<sup>-1</sup> |
| $D_{\text{fw}}$     | `D_fw`        | Diffusivity of N<sub>2</sub>O in water                                               | [Equation 5](#eq-Dfw)         |                             |
| $D_{\text{fa}}$     | `D_fa`        | Diffusivity of N<sub>2</sub>O in air                                                 | [Equation 6](#eq-Dfa)         |                             |
| $D_{\text{fa,NTP}}$ |               | Free air diffusion coefficient under standard conditions                             | [Equation 6](#eq-Dfa)         |                             |
| $n$                 | `n`           | Empirical parameter ([*1*](#ref-massman1998review))                                  | 1.81                          |                             |
| $H$                 | `H`           | Dimensionless Henry’s solubility constant                                            | [Equation 4](#eq-H)           |                             |
| $\rho$              | `rho`         | Gas density of N<sub>2</sub>O                                                        | $1.26 \times 10^6$            | mg m<sub>-3</sub>           |

Table 1: Overview of the parameters used in the model.

</div>

The diffusion fluxes between soil increments are described by Frick’s
law ([Equation 2](#eq-frick)).

<span id="eq-frick">$$F_{\text{calc}} = \frac{dC}{dZ} D_{\text s} \rho \qquad(2)$$</span>

Here, $D_s$ is the gas diffusion coefficient, $\rho$ is the gas density
of N<sub>2</sub>O, and $\frac{dC}{dZ}$ is the N<sub>2</sub>O
concentration gradient from lower to upper depth. The fluxes are
calculated based on N<sub>2</sub>O concentration gradients between
105-135 cm, 75-105 cm, 45-75 cm, 15-45 cm, and 0-15 cm depth layers, and
ambient air above the soil surface.

$\theta_w$ is the soil volumetric water content, $\theta_a$ the
air-filled porosity, and $\theta_T$ is the total soil porosity.

The gas diffusion coefficient $D_{\text s}$ was calculated according
[Equation 3](#eq-Ds) as established by Millington and Quirk in 1961
([*2*](#ref-millington1961permeability)).

<span id="eq-Ds">$$D_{\text s} = \left( \frac{\theta_w^{\frac{10}{3}} + D_{\text fw}}{H} + \theta_a^{\frac{10}{3}} \times D_{\text fa} \right) \times \theta_T^{-2} \qquad(3)$$</span>

Here, $H$ represents a dimensionless form of Henry’s solubility constant
($H'$) for N<sub>2</sub>O in water at a given temperature. The constant
$H$ for N<sub>2</sub>O is calculated as follows:

<span id="eq-H">$$H = \frac{8.5470 \times 10^5 \times \exp \frac{-2284}{\text T}}{\text R \times \text T} \qquad(4)$$</span>

Here, $\text R$ is the gas constant, and $\text T$ is the temperature
($\text T = 298 \; \text K$).

$D_{\text{fw}}$ was calculated according to [Equation 5](#eq-Dfw) as
documented by Versteeg and Van Swaaij (1988)
([*3*](#ref-versteeg1988solubility)).

<span id="eq-Dfw">$$D_{\text{fw}} = 5.07 \times 10^{-6} \times \exp \frac{-2371}{\text T} \qquad(5)$$</span>

<span id="eq-Dfa">$$D_{\text{fa}} = D_{\text{fa, NTP}} \times \left( \frac{\text T}{273.15} \right)^n \times \left( \frac{101'325}{\text P} \right) \qquad(6)$$</span>

## Smoothing curves

The N<sub>2</sub>O concentration, site preference as well as
$\delta$<sup>18</sup>O are estimated as a function of time for every
depth and every column, separately. To achieve this function
approximation, Kernel Regression as implemented in `npreg` is used
([*4*](#ref-hayfield2008nonparametric)). Besides choosing a Kernel, the
model only requires a single hyperparameter, i.e. the bandwidth (`bws`),
which facilitates the hyperparameter tuning.

<img src="results/explanation.gif" id="fig-explanation"
alt="Figure 1: Animated explanation on two examplary subsets of the data. On the left, there is a very high sinal to noise ratio, thus the optimal bandwith hyperparameter will be smaller than on the right." />

The bandwidth hyperparameter is individually tuned using 3-fold 10 times
repeated cross-validation for every combination of column and depth and
variable[^1], respectively.

<div id="tbl-hyperparameters-1">

|     |     1 |    2 |    3 |     4 |     5 |     6 |     7 |     8 |     9 |    10 |    11 |     12 |
|:----|------:|-----:|-----:|------:|------:|------:|------:|------:|------:|------:|------:|-------:|
| 7.5 | 100.0 | 13.3 | 5.32 | 83.20 | 100.0 | 94.10 | 94.10 |  5.65 |  9.80 | 21.70 | 57.70 | 100.00 |
| 30  |  65.2 | 83.2 | 6.39 |  8.15 |  65.2 |  7.67 | 83.20 | 78.30 | 14.10 | 94.10 |  6.01 |   7.67 |
| 60  |  18.1 | 18.1 | 5.65 |  6.79 |  78.3 | 24.50 | 21.70 | 17.00 |  6.79 |  6.79 |  5.65 |   6.39 |
| 90  |  18.1 | 11.1 | 8.67 |  5.32 |  14.1 | 29.40 | 15.00 |  5.00 | 10.40 | 45.20 |  7.67 |   6.01 |
| 120 |  17.0 | 10.4 | 5.00 |  5.00 |  18.1 | 11.10 |  8.15 | 10.40 |  5.65 |  6.79 |  9.21 |   6.39 |

Table 2: List of all the best working hyperparameters (bandwidth) for
every combination of depth, column for the `gN2ONha` variable. The
search range was $[5,100]$.

</div>

<div id="tbl-hyperparameters-2">

|     |     1 |    2 |    3 |     4 |     5 |     6 |     7 |    8 |     9 |   10 |   11 |   12 |
|:----|------:|-----:|-----:|------:|------:|------:|------:|-----:|------:|-----:|-----:|-----:|
| 7.5 |  8.15 | 57.7 | 21.7 |  33.3 | 100.0 |  83.2 | 37.60 | 27.7 | 18.10 | 51.0 | 54.3 | 65.2 |
| 30  | 11.10 | 33.3 | 17.0 |  23.1 | 100.0 |  40.0 | 26.10 | 51.0 | 83.20 | 26.1 | 78.3 | 78.3 |
| 60  |  7.67 | 73.7 | 35.4 | 100.0 |  69.3 |  11.8 |  5.65 |  9.8 | 94.10 | 51.0 | 11.8 | 26.1 |
| 90  | 35.40 | 94.1 | 29.4 | 100.0 |  88.5 | 100.0 | 78.30 | 73.7 |  5.32 | 40.0 | 24.5 | 11.1 |
| 120 |  8.67 | 15.0 | 31.3 |  27.7 |  88.5 |  54.3 |  7.67 | 35.4 | 73.70 | 37.6 | 12.5 | 13.3 |

Table 3: List of all the best working hyperparameters (bandwidth) for
every combination of depth, column for the `SP` variable. The search
range was $[5,100]$.

</div>

<div id="tbl-hyperparameters-3">

|     |     1 |      2 |    3 |    4 |    5 |     6 |     7 |    8 |     9 |    10 |    11 |    12 |
|:----|------:|-------:|-----:|-----:|-----:|------:|------:|-----:|------:|------:|------:|------:|
| 7.5 | 61.30 | 100.00 | 23.1 | 31.3 | 83.2 |  37.6 | 65.20 | 35.4 | 61.30 |  88.5 | 88.50 | 100.0 |
| 30  |  6.79 |   9.21 | 17.0 | 29.4 | 51.0 |  61.3 | 12.50 | 18.1 |  8.67 | 100.0 |  5.65 |  17.0 |
| 60  | 18.10 |  27.70 | 42.5 | 13.3 | 17.0 |  23.1 | 40.00 | 78.3 | 19.20 |  12.5 |  5.65 |  23.1 |
| 90  | 24.50 |  12.50 | 35.4 | 23.1 | 16.0 | 100.0 | 37.60 | 19.2 |  5.00 |  16.0 | 20.40 |  20.4 |
| 120 |  7.67 |   8.15 | 45.2 | 24.5 | 18.1 |  83.2 |  7.67 | 19.2 | 31.30 |  83.2 | 19.20 |  26.1 |

Table 4: List of all the best working hyperparameters (bandwidth) for
every combination of depth, column for the `d18O` variable. The search
range was $[5,100]$.

</div>

## State function set

Still to do.

# The data

The study uses data collected from a mesocosm experiment – i.e. an
outdoor experiment that examines the natural environment under
controlled conditions. The experiment was set up as a randomized
complete block design, with 4 varieties and 3 replicates, using 12
non-weighted lysimeters. A non-weighted lysimeter is a device to measure
the amount of water that drains through soil, and to determine the types
and amounts of dissolved nutrients or contaminants in the water. Each
lysimeter had five sampling ports with soil moisture probes and
custom-built pore gas sample, at depths of 7.5, 30, 60, 90 and 120 cm
below soil surface.

<span id="eq-dimension">$$4 \times 3 \times 5 \times 161 = 9660 \qquad(7)$$</span>

[Equation 7](#eq-dimension) shows how many observations we should expect
to have. In reality, some observations are missing.

| Code               | Name                           | Description                                                                         |
|:-------------------|:-------------------------------|:------------------------------------------------------------------------------------|
| `day_column_depth` | Combination                    |                                                                                     |
| `date_R`           | Weird date                     | Year + DOY                                                                          |
| `column`           | Column                         |                                                                                     |
| `depth`            | Measurement depth              |                                                                                     |
| `increment`        | ?                              |                                                                                     |
| `variety`          | Wheat variety                  |                                                                                     |
| `moisture`         | Soil moisture                  |                                                                                     |
| `concNO3N`         | NO<sub>3</sub>-N concentration | Nitrate nitrogen concentration (\[NO<sub>3</sub>\] = \[NO<sub>3</sub>-N\] \* 4.43). |
| `NO3N_ha`          |                                |                                                                                     |
| `corrected.N2O`    |                                |                                                                                     |
| `corrected.CO2`    |                                |                                                                                     |
| `mgN2ONm3`         |                                |                                                                                     |
| `gN2ONha`          |                                |                                                                                     |
| `gCO2Cha`          |                                |                                                                                     |
| `CN`               |                                |                                                                                     |
| `d15Nbulk`         |                                |                                                                                     |
| `d15Nalpha`        |                                |                                                                                     |
| `d15Nbeta`         |                                |                                                                                     |
| `SP`               | Site preference                |                                                                                     |
| `d18O`             |                                | Ratio of stable isotopes oxygen-18 (<sup>18</sup>O) and oxygen-16 (<sup>16</sup>O). |

# References

<div id="refs" class="references csl-bib-body" line-spacing="2">

<div id="ref-massman1998review" class="csl-entry">

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Massman, W. A review of the
molecular diffusivities of H2O, CO2, CH4, CO, O3, SO2, NH3, N2O, NO, and
NO2 in air, O2 and N2 near STP. *Atmospheric environment* **32**,
1111–1127 (1998).</span>

</div>

<div id="ref-millington1961permeability" class="csl-entry">

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Millington, R. & Quirk, J.
Permeability of porous solids. *Transactions of the Faraday Society*
**57**, 1200–1207 (1961).</span>

</div>

<div id="ref-versteeg1988solubility" class="csl-entry">

<span class="csl-left-margin">3.
</span><span class="csl-right-inline">Versteeg, G. F. & Van Swaaij, W.
P. Solubility and diffusivity of acid gases (carbon dioxide, nitrous
oxide) in aqueous alkanolamine solutions. *Journal of Chemical &
Engineering Data* **33**, 29–34 (1988).</span>

</div>

<div id="ref-hayfield2008nonparametric" class="csl-entry">

<span class="csl-left-margin">4.
</span><span class="csl-right-inline">Hayfield, T. & Racine, J. S.
[Nonparametric econometrics: The np
package](https://doi.org/10.18637/jss.v027.i05). *Journal of Statistical
Software* **27**, 1–32 (2008).</span>

</div>

</div>

[^1]: I.e. the three variables N<sub>2</sub>O concentration, site
    preference and $\delta$<sup>18</sup>O.
