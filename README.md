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
[Equation 3](#eq-MillingtonQuirk) as established by Millington and Quirk
in 1961.<sup>[1](#ref-millington1961permeability)</sup>

<span id="eq-MillingtonQuirk">$$D_{\text s} = \left( \frac{\theta_w^{\frac{10}{3}} + D_{\text fw}}{H} + \theta_a^{\frac{10}{3}} \times D_{\text fa} \right) \times \theta_T^{-2} \qquad(3)$$</span>

Here, $H$ represents a dimensionless form of Henry’s solubility constant
($H'$) for N<sub>2</sub>O in water at a given temperature. The constant
$H$ for N<sub>2</sub>O is calculated as follows:

<span id="eq-H">$$H = \frac{8.5470 \times 10^5 \times \exp \frac{-2284}{\text T}}{\text R \times \text T} \qquad(4)$$</span>

Here, $\text R$ is the gas constant, and $\text T$ is the temperature
($\text T = 298 \; \text K$).

$D_{\text{fw}}$ was calculated according to [Equation 5](#eq-Dfw) as
documented by Versteeg and Van Swaaij
(1988).<sup>[2](#ref-versteeg1988solubility)</sup>

<span id="eq-Dfw">$$D_{\text{fw}} = 5.07 \times 10^{-6} \times \exp \frac{-2371}{\text T} \qquad(5)$$</span>

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

<span id="eq-dimension">$$4 \times 3 \times 5 \times 161 = 9660 \qquad(6)$$</span>

[Equation 6](#eq-dimension) shows how many observations we should expect
to have. In reality, some observations are missing.

# References

<div id="refs" class="references csl-bib-body" line-spacing="2">

<div id="ref-millington1961permeability" class="csl-entry">

<span class="csl-left-margin">1.
</span><span class="csl-right-inline">Millington, R. & Quirk, J.
Permeability of porous solids. *Transactions of the Faraday Society*
**57**, 1200–1207 (1961).</span>

</div>

<div id="ref-versteeg1988solubility" class="csl-entry">

<span class="csl-left-margin">2.
</span><span class="csl-right-inline">Versteeg, G. F. & Van Swaaij, W.
P. Solubility and diffusivity of acid gases (carbon dioxide, nitrous
oxide) in aqueous alkanolamine solutions. *Journal of Chemical &
Engineering Data* **33**, 29–34 (1988).</span>

</div>

</div>
