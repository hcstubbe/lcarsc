# A: Example for local deployment with R/R Studio

## Installation

Clone this repository and install lcarsc from the repository's base folder including the lcarsM dependency:

``` r
devtools::install_local("dependencies/lcarsM.tar.gz")
devtools::install_local()
```

## Example
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

# B: Example for local deployment using ShinyProxy and MariaDB
This can be used as a template for local deployments.

* Clone [this](https://github.com/hcstubbe/lcarsc) repository and set the repository's folder as working directory.
* Create folders for database permanent storage:

``` bash
mkdir -p ./shinyproxy/mariadb/logs
mkdir -p ./shinyproxy/mariadb/data
```

* Start the shinyproxy using docker-compose (the docker images are pulled automatically):

``` bash
export DOCKERID=$(getent group docker | cut -d: -f3)
docker-compose -f ./shinyproxy/shinyproxy_local.yml up
```

* Open link to access shiny proxy (use test_user as username and password; see application file): [http://localhost:7070/login](http://localhost:7070/login)
