
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lcarsc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of lcarsc is to provide a lightweight clinical data
acquisition for research software.

## Installation

Clone this repository and install lcarsc from the repository's base folder including the lcarsM dependency:

``` r
devtools::install_local("dependencies/lcarsM.tar.gz")
devtools::install_local()
```

## Example

Using lcarsc locally

``` r
library(lcarsc)
lcarsc::run_app(ecrf_database_driver = RSQLite::SQLite(),
        ecrf_dbhost = "dbtest",
        ecrf_dbname = "db_test_data.sqlite3",
        config_database_driver = RSQLite::SQLite(),
        config_dbhost = "dbtest",
        config_dbname = "db_test_cfg.sqlite3",
        options = list(host = '0.0.0.0', port = 3838))
```
