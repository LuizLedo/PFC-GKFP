# GKFP / PFC Project

This repository contains the implementation of the experiments related to the **Probabilistic Fuzzy Classifier (PFC)** based on the **Gustafson–Kessel clustering algorithm with focal point (GKFP)**.

## Repository Structure

- `run_all.R`        : main script that runs all GKFP/PFC experiments
- `run_baseline.R`   : script to run baseline classifiers for comparison
- `src/`             : auxiliary functions used in the experiments
- `README.md`        : general project instructions

## Requirements

This project requires **R** (recommended: version **4.4.x**) and the following packages:

### Core packages (GKFP/PFC experiments)

- MASS  
- dplyr  
- clue  

### Baseline models (`run_baseline.R`)

- e1071  
- rpart.plot  
- caret  
- ggplot2  
- lattice  

## Installation

Install all required packages in R:

```r
install.packages(c(
  "MASS", "dplyr", "clue",
  "e1071", "rpart.plot", "caret", "ggplot2", "lattice"
))

