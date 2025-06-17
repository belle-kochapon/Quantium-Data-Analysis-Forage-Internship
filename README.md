# Quantium Data Analysis - Forage Internship

This repository contains R scripts developed for the **Quantium Data Analysis Virtual Internship Program** via **Forage.com**. It addresses two core data analysis tasks:

## Project Overview

This internship project simulates a data analyst's role at Quantium, focusing on:

1.  **Task 1: EDA & Data Pre-processing:** Understanding and cleaning transaction/customer data to gain insights into chip purchasing behavior.
2.  **Task 2: Trial Store Analysis:** Identifying control stores and assessing the impact of a trial on sales and customer numbers.

## Files

* `task1_solution.R`: R script for data loading, cleaning, and EDA.
* `task2_solution.R`: R script for control store selection and trial assessment.
* `task1_solution.pdf`: PDF report for Task 1 (code output, visualizations).
* `task2_solution.pdf`: PDF report for Task 2 (code output, visualizations).

## Setup

### Prerequisites

* R ([CRAN](https://cran.r-project.org/))
* RStudio ([RStudio website](https://posit.co/download/rstudio-desktop/))

### Required Packages

```R
install.packages("data.table")
install.packages("ggplot2")
install.packages("tidyr")
# Add any other packages explicitly used in the scripts if applicable
```

### Data

* Datasets (e.g., `transaction_data.csv`) are **not included** due to program restrictions.
* Update `filePath` in scripts to your data directory: `filePath <- "C:/Your/Path/To/ForageData"`

### How to Run

1.  **Clone Repo:** `git clone [your-repository-url]`
2.  **Place Data:** Ensure Forage.com data files are in your `filePath` directory.
3.  **Run Scripts:** Open `.R` files in RStudio and execute.

## Structure

```
.
├── task1_solution.R
├── task2_solution.R
├── task1_solution.pdf
├── task2_solution.pdf
└── README.md
```

## Disclaimer

Solutions developed for a virtual internship. Proprietary Quantium/Forage data is not included.
