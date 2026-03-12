# ESOT — Episodes of State Ownership Transformation

An R package to identify, explore, and visualize **Episodes of State Ownership Transformation (ESOT)** using V-Dem data. The package detects **privatization** (movement towards a market economy) and **statization** (movement towards greater state ownership) episodes based on the V-Dem variable `v2clstown_osp` (State Ownership of the Economy, 0–4 scale).

> **Note:** This package is built upon the methodology and codebase of the **[ERT — Episodes of Regime Transformation](https://github.com/vdeminstitute/ERT)** package, developed by the [V-Dem Institute](https://www.v-dem.net/). The episode-detection algorithm is directly adapted from ERT's approach to identifying regime transitions, scaled and re-oriented for economic ownership dynamics. Full credit for the original framework goes to the ERT authors (see [Citation](#citation)).

---

## Installation

```r
# Install the development version from GitHub
# (requires the devtools package)
install.packages("devtools")
devtools::install_github("your-username/ESOT")
```

> Make sure you have an up-to-date R installation and the appropriate build tools:
> - **Windows**: [Rtools](https://cran.r-project.org/bin/windows/Rtools/)
> - **macOS**: Xcode Command Line Tools
> - **Linux**: `r-base-dev`

---

## The State Ownership Index

The package uses the V-Dem variable **`v2clstown_osp`** (State Ownership of the Economy), which ranges from **0** (high state ownership / planned economy) to **4** (low state ownership / market economy).

Economy types are derived as follows:

| Value range        | Economy type                    |
|--------------------|---------------------------------|
| `v2clstown_osp < 1`  | Planned economy (0)            |
| `1 ≤ v2clstown_osp < 2` | State-dominated mixed economy (1) |
| `2 ≤ v2clstown_osp < 3` | Market-dominated mixed economy (2) |
| `v2clstown_osp ≥ 3`  | Market economy (3)             |

---

## Functions

### `get_eps()`
Identifies episodes of privatization and statization for all countries in the most recent V-Dem dataset.

```r
# Get episodes with default parameters
episodes <- get_eps()

# Customize thresholds
episodes <- get_eps(start_incl = 0.04,
                    cum_incl   = 0.4,
                    year_turn  = 0.12,
                    cum_turn   = 0.4,
                    tolerance  = 5)
```

**Parameters** (all scaled to the 0–4 range of `v2clstown_osp`, following ERT's methodology):

| Parameter    | Description                                                          | Default |
|--------------|----------------------------------------------------------------------|---------|
| `start_incl` | Minimum annual change to trigger an episode onset                   | `0.04`  |
| `cum_incl`   | Minimum cumulative change to qualify as a manifest episode          | `0.4`   |
| `year_turn`  | Annual change in opposite direction to trigger episode termination  | `0.12`  |
| `cum_turn`   | Cumulative change in opposite direction to trigger termination      | `0.4`   |
| `tolerance`  | Number of years of tolerance for stasis or reverse movement         | `5`     |

**Episode outcomes — Privatization:**

| Code | Outcome                     |
|------|-----------------------------|
| 1    | Market transition           |
| 2    | Preempted market transition |
| 3    | Stabilized planned economy  |
| 4    | Reverted privatization      |
| 5    | Deepened market economy     |
| 6    | Uncertain outcome           |

**Episode outcomes — Statization:**

| Code | Outcome                    |
|------|----------------------------|
| 1    | Market collapse            |
| 2    | Preempted market collapse  |
| 3    | Diminished market economy  |
| 4    | Averted statization        |
| 5    | Deepened planned economy   |
| 6    | Uncertain outcome          |

---

### `find_overlap()`
Checks for and reports overlapping privatization and statization episodes.

```r
overlap <- find_overlap()
```

---

### `plot_episodes()`
Plots privatization and statization episodes over time for a selected country.

```r
# Plot episodes for Russia between 1950 and 2010
plot_episodes(country = "Russia", years = c(1950, 2010))

# Plot in Spanish
plot_episodes(country = "Venezuela", years = c(1960, 2023), lang = "es")
```

---

### `plot_all()`
Plots the global number or share of countries undergoing privatization or statization episodes per year.

```r
# Absolute number of countries
plot_all(abs = TRUE, years = c(1950, 2023))

# Share of countries (%)
plot_all(abs = FALSE, years = c(1950, 2023))

# In Spanish
plot_all(lang = "es")
```

---

## Localization

Both `plot_episodes()` and `plot_all()` support localized labels via the `lang` argument:

| Language | Code  |
|----------|-------|
| English  | `"en"` (default) |
| Spanish  | `"es"` |

---

## Citation

If you use the ESOT package, please cite both this package and the original ERT framework on which it is based:

**ESOT package:**
> Maerz, Seraphine, Amanda Edgell, Joshua Krusell, Laura Maxwell, and Sebastian Hellmeier. *ESOT — Episodes of State Ownership Transformation R package*. 2025.

**Original ERT framework (required credit):**
> Maerz, Seraphine, Amanda Edgell, Joshua Krusell, Laura Maxwell, and Sebastian Hellmeier. *ERT — Episodes of Regime Transformation R package*. Varieties of Democracy (V-Dem) Project. 2025. <https://github.com/vdeminstitute/ERT>

**V-Dem dataset:**
> Coppedge, Michael et al. *V-Dem Dataset*. Varieties of Democracy (V-Dem) Project. <https://www.v-dem.net>

---

## License

GPL-3
