# For local deployment

Clone this repository and run from base directory.

## Create folders for database permanent storage

``` bash
mkdir -p ./mariadb/logs
mkdir -p ./mariadb/data
```

## Start the shinyproxy using docker-compose

``` bash
export DOCKERID=$(getent group docker | cut -d: -f3)
docker-compose -f ./shinyproxy/shinyproxy_local.yml up
```
