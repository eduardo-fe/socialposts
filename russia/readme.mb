# Russia and Its Peers: GDP per Capita Counterfactuals (2000–2024)

This repository contains an exploratory data analysis of GDP per capita trajectories across former Soviet countries, with a focus on **Russia’s relative performance since 2000**.

The project combines descriptive visualisation with simple counterfactual benchmarks to answer one question:

> *Where would Russia be today if it had grown like its peers?*

This is **not** a causal analysis. The counterfactuals are transparent growth benchmarks designed to illustrate the magnitude of divergence over time.

---

## Overview

Using World Bank GDP per capita data (constant prices), the analysis proceeds in three steps:

1. **Data preparation**
   - Pull GDP per capita data (constant prices) for 15 former Soviet republics using the `WDI` package
   - Clean, harmonise, and group countries into geopolitical areas

2. **Index construction and visualisation**
   - Rebase GDP per capita series to a common reference year (index = 100)
   - Plot trajectories:
     - by country
     - by geopolitical area
     - by sub-periods (2000–2013, 2014–2024)

3. **Counterfactual analysis**
   - Compute average annual growth rates by country (2000–2024)
   - Construct counterfactual GDP per capita paths for Russia assuming it had grown at:
     - the average rate of former Soviet countries (excluding the Caucasus)
     - the average rate of the best-performing countries (excluding Russia)
     - the average rate of the worst-performing countries (excluding Russia)
   - Compare realised Russia to these benchmarks

---

## Key Findings (Descriptive)

- From 2000 to 2013, Russia’s growth broadly tracked that of its peers, especially during the commodity-driven expansion of the 2000s.
- From around 2014 onwards, Russia’s growth path visibly flattens.
- By 2024, Russia’s GDP per capita lies much closer to benchmarks based on the weakest performers than to the regional average or best-case paths.
- Small differences in annual growth rates compound into large gaps in living standards over two decades.

---

## What This Is (and Is Not)

**This is:**
- a transparent benchmarking exercise
- a compounding-growth illustration
- a tool for framing discussion

**This is not:**
- a causal estimate
- a structural growth model
- a policy evaluation

The counterfactuals answer *“what if growth rates had been different?”*, not *“why growth rates were different.”*

---

## Data

- **Source:** World Bank, via the `WDI` R package  
- **Indicator:** GDP per capita, constant prices (`NY.GDP.PCAP.KD`)
- **Coverage:** 2000–2024 (or latest available year)

---

## Requirements

R packages used:

```r
WDI
dplyr
tidyr
ggplot2
stringr
RColorBrewer
