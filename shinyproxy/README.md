# For local testing

## Start the shinyproxy using docker-compose
```
export DOCKERID=$(getent group docker | cut -d: -f3)
docker-compose -f ./shinyproxy/shinyproxy_docker.yml up
```