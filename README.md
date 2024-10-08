
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCARS-C <img src="man/figures/logo.png" align="right" width=160 height=160 alt="" />

LCARS-C is a lightweight clinical data acquisition and management
software for clinical research and other data-focused research projects.
It is based on [R Shiny](https://github.com/rstudio/shiny) and the
[golem](https://github.com/ThinkR-open/golem) framework.

## ✨ New version: LCARS-M2 ✨

Please refer to the new version LCARS mark-II (LCARS-M2) [here](https://github.com/hcstubbe/lcars-m2). The new version was designed using [Python](https://www.python.org/)/[Django](https://www.djangoproject.com/). It incorporates the experience and learning points from the LCARS-C prototype.

## Installation

Install from github:

``` r
install.packages("devtools")
devtools::install_github("hcstubbe/lcarsc")
```

Alternatively, clone this repository and install lcarsc from the
repository’s base folder including the lcarsM dependency:

``` r
install.packages("devtools")
devtools::install_local("dependencies/lcarsM.tar.gz")
devtools::install_local()
```

## Example

Using LCARS-C locally: In this example, the config and scientific
databases are saved in the working directory as
‘db_test_data.sqlite3.sqlite3’ and ‘db_test_cfg.sqlite3.sqlite3’. The
preview data is saved as ‘db_preview.sqlite3’. ‘db_preview.sqlite3’ is
overwritten each time the software reboots or the page reloads.

``` r
library(lcarsc)
lcarsc::run_app(ecrf_database_driver = RSQLite::SQLite(),
        ecrf_dbhost = "dbtest",
        ecrf_dbname = "db_test_data.sqlite3",
        ecrf_dbuser = "test_user",
        ecrf_dbpassword = NULL,
        config_database_driver = RSQLite::SQLite(),
        config_dbpassword = NULL,
        config_dbhost = "dbtest",
        config_dbname = "db_test_cfg.sqlite3",
        config_dbuser = "test_user",
        options = list(host = '0.0.0.0', port = 3838))
```

## Furter examples
Please refer to this [link](https://github.com/hcstubbe/lcarsc/tree/master/shinyproxy) for further configuration examples.
