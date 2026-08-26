# Disaster Damage and Response Estimation: public demonstration

An automated reporting pipeline that turns a cyclone scenario into a costed,
sector-by-sector damage and response assessment for every area council in a
country. Built in R and Quarto for the Government of Vanuatu by
[Théophile L. Mouton](https://github.com/TheophileMt92) and
[Yan Holtz](https://github.com/holtzy).

**[View the report](https://theophilemt92.github.io/vanuatu-disaster-report-demo/)**

---

## Every figure in this repository is simulated

This is a public demonstration of the pipeline, not a publication of its
results. The real assessment was produced for the Government of Vanuatu from
official statistics and remains with the client.

Everything in `data/` here is **synthetic data generated from scratch** by
[`scripts/simulate_data.R`](scripts/simulate_data.R). No real value was read, rescaled or
perturbed to produce it: the script reads only the *structure* of the original
inputs (geography, indicator and attribute names, units, sources, years) and
then generates values by giving each area council a synthetic population weight
and distributing plausible national totals across councils.

The numbers therefore do not describe Vanuatu and must not be cited or reused
as if they did. What is real is everything around them: the geography, the
indicator taxonomy, the sector coverage, and every line of calculation,
aggregation and presentation logic.

[`scripts/audit_render.R`](scripts/audit_render.R) is the check that enforces this. It extracts
every embedded data payload from the rendered page and confirms that none of it
matches the original report, comparing observed agreement against the agreement
expected by chance rather than against an arbitrary threshold.

## What the pipeline does

The input is a scenario file: one row per area council, giving the cyclone
category (2 to 5) that council experiences.

```csv
National,Province,Area Council,Hazard,Intensity
Vanuatu,Shefa,Port Vila,Cyclone,4
Vanuatu,Tafea,North Tanna,Cyclone,3
```

Change that file, re-render, and the entire assessment recomputes. Nothing else
is touched by hand.

For each of ten sectors the report works through the same four stages:

1. **Baseline**: what exists, per area council
2. **Damage**: what the scenario destroys, via category-specific multipliers
3. **Response**: what relief that implies, in physical units
4. **Financial**: what it costs to replace, in vatu

Sectors covered: Education, Emergency Telecommunications, Energy, Food Security,
Gender & Protection, Health, Logistics, Shelter, WASH, and Business.

Results are aggregated from area council to province to national level in a
single pass, and presented as interactive sortable tables and choropleth maps,
with every table also written to `output/` as CSV for downstream use.

## Scale

| | |
|---|---|
| Source document | ~6,700 lines of Quarto, 62 R chunks |
| Geography | 71 area councils, 6 provinces |
| Sectors | 10 |
| Indicators | 38, across 164 attributes |
| Interactive tables | 44 |
| Choropleth maps | 13 |
| CSV exports | 46 |

The report also runs its own quality checks, validating council names against
the scenario config and flagging missing baselines before any estimate is
computed, then re-checking the outputs at the end.

## Reproducing it

Requires R with `dplyr`, `tidyr`, `reactable`, `htmltools`, `readxl`, `here`,
`sf` and `leaflet`, plus [Quarto](https://quarto.org).

```bash
git clone https://github.com/TheophileMt92/vanuatu-disaster-report-demo.git
cd vanuatu-disaster-report-demo
quarto render index.qmd
```

`simulate_data.R` regenerates `data/` and needs the original inputs, so it is
included as documentation of method rather than as a step you can run. The
synthetic data it produced is committed, so the render works as-is. It is
seeded, so the figures are stable across renders.

## Repository layout

```
index.qmd                        the pipeline
data/
  baseline_indicators.csv        what exists, per council: 38 indicators, 164 attributes
  damage_multipliers.csv         proportion of each asset lost, per cyclone category
  response_resources.csv         relief items issued per affected unit
  unit_costs.csv                 replacement cost per unit, in vatu
  hazard_scenario.csv            the scenario: a cyclone category per council
  council_province_lookup.csv    council to province mapping
  GIS_layers/area_councils.geojson   council boundaries
assets/                          report styling
output/                          generated CSVs, one per table
scripts/
  simulate_data.R                how the synthetic data was made
  patch_qmd.R                    the changes between this demo and the client report
  audit_render.R                 the check that no real figure survives
```

The `scripts/` folder documents how this demonstration was derived from the
client project. It is not part of the pipeline: `index.qmd` reads only `data/`
and renders without any of it.

## Licence and credit

Pipeline and report by Théophile L. Mouton and Yan Holtz. Published with the
agreement of both authors. The underlying assessment was commissioned by the
Government of Vanuatu; nothing belonging to that engagement is reproduced here.
