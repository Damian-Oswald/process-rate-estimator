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

        Depth Column Variable  Bandwidth
    1     7.5      1  gN2ONha 100.000000
    2    30.0      1  gN2ONha  65.183634
    3    60.0      1  gN2ONha  18.053204
    4    90.0      1  gN2ONha  18.053204
    5   120.0      1  gN2ONha  16.982540
    6     7.5      2  gN2ONha  13.298300
    7    30.0      2  gN2ONha  83.242498
    8    60.0      2  gN2ONha  18.053204
    9    90.0      2  gN2ONha  11.069837
    10  120.0      2  gN2ONha  10.413329
    11    7.5      3  gN2ONha   5.315225
    12   30.0      3  gN2ONha   6.385230
    13   60.0      3  gN2ONha   5.650323
    14   90.0      3  gN2ONha   8.668315
    15  120.0      3  gN2ONha   5.000000
    16    7.5      4  gN2ONha  83.242498
    17   30.0      4  gN2ONha   8.154232
    18   60.0      4  gN2ONha   6.787786
    19   90.0      4  gN2ONha   5.315225
    20  120.0      4  gN2ONha   5.000000
    21    7.5      5  gN2ONha 100.000000
    22   30.0      5  gN2ONha  65.183634
    23   60.0      5  gN2ONha  78.305717
    24   90.0      5  gN2ONha  14.136691
    25  120.0      5  gN2ONha  18.053204
    26    7.5      6  gN2ONha  94.069399
    27   30.0      6  gN2ONha   7.670637
    28   60.0      6  gN2ONha  24.508258
    29   90.0      6  gN2ONha  29.442003
    30  120.0      6  gN2ONha  11.069837
    31    7.5      7  gN2ONha  94.069399
    32   30.0      7  gN2ONha  83.242498
    33   60.0      7  gN2ONha  21.687485
    34   90.0      7  gN2ONha  15.027938
    35  120.0      7  gN2ONha   8.154232
    36    7.5      8  gN2ONha   5.650323
    37   30.0      8  gN2ONha  78.305717
    38   60.0      8  gN2ONha  16.982540
    39   90.0      8  gN2ONha   5.000000
    40  120.0      8  gN2ONha  10.413329
    41    7.5      9  gN2ONha   9.795756
    42   30.0      9  gN2ONha  14.136691
    43   60.0      9  gN2ONha   6.787786
    44   90.0      9  gN2ONha  10.413329
    45  120.0      9  gN2ONha   5.650323
    46    7.5     10  gN2ONha  21.687485
    47   30.0     10  gN2ONha  94.069399
    48   60.0     10  gN2ONha   6.787786
    49   90.0     10  gN2ONha  45.167783
    50  120.0     10  gN2ONha   6.787786
    51    7.5     11  gN2ONha  57.681335
    52   30.0     11  gN2ONha   6.006547
    53   60.0     11  gN2ONha   5.650323
    54   90.0     11  gN2ONha   7.670637
    55  120.0     11  gN2ONha   9.214809
    56    7.5     12  gN2ONha 100.000000
    57   30.0     12  gN2ONha   7.670637
    58   60.0     12  gN2ONha   6.385230
    59   90.0     12  gN2ONha   6.006547
    60  120.0     12  gN2ONha   6.385230
    61    7.5      1       SP   8.154232
    62   30.0      1       SP  11.069837
    63   60.0      1       SP   7.670637
    64   90.0      1       SP  35.368956
    65  120.0      1       SP   8.668315
    66    7.5      2       SP  57.681335
    67   30.0      2       SP  33.271365
    68   60.0      2       SP  73.661717
    69   90.0      2       SP  94.069399
    70  120.0      2       SP  15.027938
    71    7.5      3       SP  21.687485
    72   30.0      3       SP  16.982540
    73   60.0      3       SP  35.368956
    74   90.0      3       SP  29.442003
    75  120.0      3       SP  31.298173
    76    7.5      4       SP  33.271365
    77   30.0      4       SP  23.054771
    78   60.0      4       SP 100.000000
    79   90.0      4       SP 100.000000
    80  120.0      4       SP  27.695915
    81    7.5      5       SP 100.000000
    82   30.0      5       SP 100.000000
    83   60.0      5       SP  69.293134
    84   90.0      5       SP  88.490517
    85  120.0      5       SP  88.490517
    86    7.5      6       SP  83.242498
    87   30.0      6       SP  39.969205
    88   60.0      6       SP  11.767734
    89   90.0      6       SP 100.000000
    90  120.0      6       SP  54.260485
    91    7.5      7       SP  37.598791
    92   30.0      7       SP  26.053381
    93   60.0      7       SP   5.650323
    94   90.0      7       SP  78.305717
    95  120.0      7       SP   7.670637
    96    7.5      8       SP  27.695915
    97   30.0      8       SP  51.042512
    98   60.0      8       SP   9.795756
    99   90.0      8       SP  73.661717
    100 120.0      8       SP  35.368956
    101   7.5      9       SP  18.053204
    102  30.0      9       SP  83.242498
    103  60.0      9       SP  94.069399
    104  90.0      9       SP   5.315225
    105 120.0      9       SP  73.661717
    106   7.5     10       SP  51.042512
    107  30.0     10       SP  26.053381
    108  60.0     10       SP  51.042512
    109  90.0     10       SP  39.969205
    110 120.0     10       SP  37.598791
    111   7.5     11       SP  54.260485
    112  30.0     11       SP  78.305717
    113  60.0     11       SP  11.767734
    114  90.0     11       SP  24.508258
    115 120.0     11       SP  12.509631
    116   7.5     12       SP  65.183634
    117  30.0     12       SP  78.305717
    118  60.0     12       SP  26.053381
    119  90.0     12       SP  11.069837
    120 120.0     12       SP  13.298300
    121   7.5      1     d18O  61.317853
    122  30.0      1     d18O   6.787786
    123  60.0      1     d18O  18.053204
    124  90.0      1     d18O  24.508258
    125 120.0      1     d18O   7.670637
    126   7.5      2     d18O 100.000000
    127  30.0      2     d18O   9.214809
    128  60.0      2     d18O  27.695915
    129  90.0      2     d18O  12.509631
    130 120.0      2     d18O   8.154232
    131   7.5      3     d18O  23.054771
    132  30.0      3     d18O  16.982540
    133  60.0      3     d18O  42.489062
    134  90.0      3     d18O  35.368956
    135 120.0      3     d18O  45.167783
    136   7.5      4     d18O  31.298173
    137  30.0      4     d18O  29.442003
    138  60.0      4     d18O  13.298300
    139  90.0      4     d18O  23.054771
    140 120.0      4     d18O  24.508258
    141   7.5      5     d18O  83.242498
    142  30.0      5     d18O  51.042512
    143  60.0      5     d18O  16.982540
    144  90.0      5     d18O  15.975374
    145 120.0      5     d18O  18.053204
    146   7.5      6     d18O  37.598791
    147  30.0      6     d18O  61.317853
    148  60.0      6     d18O  23.054771
    149  90.0      6     d18O 100.000000
    150 120.0      6     d18O  83.242498
    151   7.5      7     d18O  65.183634
    152  30.0      7     d18O  12.509631
    153  60.0      7     d18O  39.969205
    154  90.0      7     d18O  37.598791
    155 120.0      7     d18O   7.670637
    156   7.5      8     d18O  35.368956
    157  30.0      8     d18O  18.053204
    158  60.0      8     d18O  78.305717
    159  90.0      8     d18O  19.191367
    160 120.0      8     d18O  19.191367
    161   7.5      9     d18O  61.317853
    162  30.0      9     d18O   8.668315
    163  60.0      9     d18O  19.191367
    164  90.0      9     d18O   5.000000
    165 120.0      9     d18O  31.298173
    166   7.5     10     d18O  88.490517
    167  30.0     10     d18O 100.000000
    168  60.0     10     d18O  12.509631
    169  90.0     10     d18O  15.975374
    170 120.0     10     d18O  83.242498
    171   7.5     11     d18O  88.490517
    172  30.0     11     d18O   5.650323
    173  60.0     11     d18O   5.650323
    174  90.0     11     d18O  20.401286
    175 120.0     11     d18O  19.191367
    176   7.5     12     d18O 100.000000
    177  30.0     12     d18O  16.982540
    178  60.0     12     d18O  23.054771
    179  90.0     12     d18O  20.401286
    180 120.0     12     d18O  26.053381

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
