# Exploring Drug Identification Numbers (DINs), Health Canada's Drug Product Database (DPD), and WHO Anatomical Therapeutic Chemical (ATC) Classification [beta]

This repository contains three R Shiny apps for exploring Health Canada and WHO ATC data.

Important: this is a beta version. Expect ongoing changes in features, outputs, and UI details. If you have any feedback, please leave a comment in GitHub or email the author (Martin Ho) at martinkh.ho@mail.utoronto.ca

## Quick Start (for users new to `renv` and Shiny)

1. Open any of the three app scripts in RStudio (`rdinexplore.R`, `rdpdexplore.R`, or `ratcexplore.R`).
2. In the RStudio Console, run:

```r
renv::restore()
```

3. Click **Run App** in RStudio (top-right of the script editor).
4. This will open the app locally on your browser. When done, close the browser tab and click Stop or hit Esc in the RStudio console.

Notes:
- `renv::restore()` installs the exact package versions recorded in `renv.lock`.
- The first restore can take several minutes.

## App: `rdinexplore.R`

Purpose: create a DIN-focused output by combining DPD + CIHI data and filtering by ATC and user options.

High-level flow:
1. Choose whether to download latest or use existing cached DPD and CIHI formulary data.
2. Choose ATC dictionary source (cached vs upload).
3. Optionally upload a DIN list to find similar DINs.
4. Select ATCs.
5. Choose search scope and filters.
6. Review drugs with missing ATC (optional append).
7. Review results and download outputs.

Note for ICES researchers:
- `rdinexplore.R` should not be used alone. Instead, it complements the `%dinexplore` SAS macro. `%dinexplore` often includes too many irrelevant DINs which requires substantial manual effort to clean. `rdinexplore.R` complements that by often including too few DINs - but every DIN it includes is likely correct.
- It is not clear yet which option is preferable on Page 4 (Search all drugs versus Search publicly covered drugs). For now, it would be really appreciated if you can try both and provide feedback on what seems to work better.
- To ensure that `rdinexplore.R` is not too conservative, it is recommended to select the following options on Page 5:
  - Questions 1-3: Do not make any restrictions.
  - Question 4: Select both PseudoDINs and Unmapped DINs

Outputs:
- Downloadable files from the app:
  - summary table CSV
  - DIN list CSV
- Automatically saved run outputs under:
  - `output/runs_rdinexplore/YYYY/MM/DD/<run_id>/`

## App: `rdpdexplore.R`

Purpose: quickly load and flatten DPD data, then export a combined DPD file with a summary.

High-level flow:
1. Choose DPD source (download latest vs use existing cached).
2. Review results (rows, columns, key notes), then download.

Outputs:
- Downloadable files from the app:
  - summary table CSV
  - combined DPD CSV
- Automatically saved run outputs under:
  - `output/runs_rdpdexplore/YYYY/MM/DD/<run_id>/`

## App: `ratcexplore.R`

Purpose: explore and export selected WHO ATC codes using either a hierarchy checklist or searchbar workflow.

High-level flow:
1. Choose viewer mode (`Checklist` or `Searchbar`).
2. Choose ATC dictionary source (cached vs upload CSV).
3. Select ATCs:
   - Checklist mode: nested hierarchy with expand/collapse.
   - Searchbar mode: level-specific multi-select filters (ATC1 to ATC4), with optional `Explode ATCs`.
4. Review results page summary and selected ATCs, then download.

Outputs:
- Downloadable files from the app:
  - summary table CSV
  - selected ATCs CSV
- Automatically saved run outputs under:
  - `output/runs_ratcexplore/YYYY/MM/DD/<run_id>/`

## Logging

All apps write daily logs under:

- `log/YYYY/MM/DD/`

Each app writes to its own log file prefix:
- `rdinexplore_*.log`
- `rdpdexplore_*.log`
- `ratcexplore_*.log`

## License

This repository uses a dual-license approach:

- Code (`.R` files and other software/source code): **PolyForm Noncommercial License 1.0.0**
  - See `LICENSE-POLYFORM-NONCOMMERCIAL-1.0.0.txt`
- Non-code content (documentation and other creative content): **Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)**
  - See `LICENSE-CC-BY-NC-SA-4.0.txt`
