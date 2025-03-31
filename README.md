
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tveTableTool

<!-- badges: start -->

[![R-CMD-check](https://github.com/thevalueengineers/tveTableTool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thevalueengineers/tveTableTool/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/thevalueengineers/tveTableTool/graph/badge.svg?token=NOH2EZFRH6)](https://codecov.io/gh/thevalueengineers/tveTableTool)
<!-- badges: end -->

The goal of tveTableTool is to provide flexible and powerful tools for
creating cross-tabulation tables from survey data. It handles both
labeled and unlabeled data, supports weighting, and provides various
formatting options for analyzing and presenting survey results.

## Features

- Generate cross-tabulation tables with multiple row variables and a
  column variable
- Support for different variable types (numeric, single-code,
  multi-code)
- Apply weighting to calculations
- Calculate percentages (column or row) or display raw counts
- Add index values to tables
- Handle both labeled and unlabeled data
- Automatic detection of variable types

## Installation

Install from github:

``` r
renv::install("thevalueengineers/tveTableTool")
```

## Basic Usage

### Generating Tables

The main function for creating tables is `generate_table()`:

``` r
library(tveTableTool)

# Load example data
data(testTableData)

# Generate a basic table
table <- generate_table(
  dat = testTableData,
  row_vars = c("gender", "age_group"),
  col_var = "region",
  weight_var = "no_weight"
)

# View the table
head(table)
```

### Adding Index Values

You can add index values to your tables with `add_index()`:

``` r
# Add index values to the table
indexed_table <- add_index(table)

# View the indexed table
head(indexed_table)
```

### Working with Unlabeled Data

For data without variable labels, use the unlabeled version of the
function:

``` r
# Generate a table from unlabeled data
unlabeled_table <- generate_table_unlabs(
  dat = my_unlabeled_data,
  row_vars = c("gender", "age"),
  col_var = "region",
  weight_var = "weight_var"
)
```

## Example with Test Data

Here’s a complete example using the included test data:

``` r
library(tveTableTool)
library(dplyr)

# Load the test data
data(testTableData)

# Examine the data structure
glimpse(testTableData)

# Create a table with gender as rows and region as columns
# Using the no_weight variable (no weighting)
basic_table <- generate_table(
  dat = testTableData,
  row_vars = "gender",
  col_var = "region",
  weight_var = "no_weight"
)

# Display the table
basic_table

# Create a weighted table using gender_weight1
weighted_table <- generate_table(
  dat = testTableData,
  row_vars = c("gender", "age_group"),
  col_var = "region",
  weight_var = "gender_weight1"
)

# Add index values
indexed_table <- add_index(weighted_table)

# Display the indexed table
indexed_table
```
