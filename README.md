# Process Rate Estimator
Damian Oswald
2023-09-23

- [<span class="toc-section-number">1</span>
  Introduction](#introduction)
- [<span class="toc-section-number">2</span> Formal model
  description](#formal-model-description)
- [References](#references)

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

The diffusion fluxes between soil increments are described by Frick’s
law ([Equation 2](#eq-frick)).

<span id="eq-frick">$$F_{\text{calc}} = \frac{dC}{dZ} D_{\text s} \rho \qquad(2)$$</span>

Here, $D_s$ is the gas diffusion coefficient, $\rho$ is the gas density
of N<sub>2</sub>O, and $\frac{dC}{dZ}$ is the N<sub>2</sub>O
concentration gradient from lower to upper depth.

The gas diffusion coefficient $D_{\text s}$ was calculated according
[Equation 3](#eq-MillingtonQuirk) as established by Millington and Quirk
in 1961.<sup>[1](#ref-millington1961permeability)</sup>

<span id="eq-MillingtonQuirk">$$D_{\text s} = \left( \frac{\theta_w^{\frac{10}{3}} + D_{\text fw}}{H} + \theta_a^{\frac{10}{3}} \times D_{\text fa} \right) \times \theta_T^{-2} \qquad(3)$$</span>

Here, $H$ represents a dimensionless form of Henry’s solubility constant
($H'$) for N<sub>2</sub>O in water at a given temperature. The constant
$H$ for N<sub>2</sub>O is calculated as follows:

<span id="eq-H">$$H = \frac{8.5470 \times 10^5 \times \exp \frac{-2284}{\text T}}{\text R \times \text T} \qquad(4)$$</span>

Here, $\text R$ is the gas constant, and $\text T$ is the temperature
($\text T = 298 \; \text K$).

$D_{\text{fw}}$ was calculated according to **?@eq-Dfw** as documented
by Versteeg and Van Swaaij (1988).

$$D_{\text{fw}} = 5.07 \times 10^{-6} \times \exp \frac{-2371}{\text T}$$\#eq-Dfw

# References

<div id="refs" class="references csl-bib-body" line-spacing="2">

<div id="ref-millington1961permeability" class="csl-entry">

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Millington, R. & Quirk, J.
Permeability of porous solids. *Transactions of the Faraday Society*
**57**, 1200–1207 (1961).</span>

</div>

</div>
