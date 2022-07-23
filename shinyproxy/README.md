# For testing local deployment
This can be used as a template for local deployments.

* Clone this repository and run from base directory.
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
