可以直接补上一个简洁的 README 说明，和这份脚本配套上传。

## README overview

**GeoDetector with country-specific discretization**

This repository provides an R workflow for running country-level GeoDetector analysis with country-specific discretization. The script calculates:

* single-factor q values for each country
* pairwise interaction q values for each country
* interaction types based on the relationship between q1, q2, and q12
* a log of the discretization strategy applied to each variable

The analysis is designed for datasets where observations are nested within countries and all missing values have already been removed before running the script.

## Input data requirements

The input file should be placed at:

`data/input/geodetector_input.xlsx`

The dataset must contain:

* one country identifier column
* one response variable
* multiple explanatory variables

In the current script, the required columns are:

* `country`
* `T_00_10`
* `dem`
* `slope`
* `gdp10`
* `pop10`
* `pre10`
* `tem10`
* `urban10`
* `npp10`
* `PArate`

All explanatory variables should be numeric or convertible to numeric.

## Output

The script writes one Excel file to:

`outputs/geodetector_country_q_and_interaction.xlsx`

This file contains three sheets:

* `factor_q`: country-level single-factor q values and p-values
* `interaction_q`: country-level pairwise interaction q values and interaction types
* `discretization_log`: country-level discretization settings used for each variable

## Methods summary

The workflow applies the following steps:

1. Read and clean the input data
2. Check that no missing values remain
3. Filter countries with too few observations
4. Discretize explanatory variables within each country
5. Run the GeoDetector factor detector
6. Run the GeoDetector interaction detector
7. Export results to Excel

Different discretization strategies are used depending on variable type:

* quantile-based discretization for most continuous variables
* log1p transformation followed by quantile discretization for skewed variables
* separate zero class plus quantile discretization for zero-inflated variables

## Notes

* This script assumes that all missing values have already been removed.
* Countries with fewer than the minimum required observations are excluded.
* Variables that cannot be discretized within a country are skipped for that country.
* Interaction results are only calculated when the country sample size and valid variable count meet the minimum thresholds.

## Reproducibility

To reproduce the analysis:

1. Place the input file in the `data/input/` folder
2. Adjust variable names and parameter settings in the script if needed
3. Run the R script
4. Check the output Excel file in the `outputs/` folder

If you want, I can also give you a **complete GitHub-ready README.md** version with:

* project title
* folder structure
* package requirements
* example command
* citation text
