# R-code-MT-Historical-variation-Swedish-bumblebee-queens
R code for master's thesis "Historical variation in queen body size of Swedish bumblebees"


# Bombus queen ITD analysis

This folder is an R script for reproducing the thesis analyses.

## File

- `bombus_thesis_analysis_ordered.R`

## How to run

1. Put your raw ITD Excel file and spring climate CSV file in the same folder as the script.
2. Open the script in RStudio.
3. Edit these two lines near the top:

```r
itd_file <- "Bombus_ITD_AN.xlsx"
climate_file <- "climate_spring_regions.csv"
```

4. Run the script from top to bottom.
5. Outputs are written to an `outputs/` folder.


